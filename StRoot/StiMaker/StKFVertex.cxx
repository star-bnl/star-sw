// $Id: StKFVertex.cxx,v 2.5 2015/12/20 01:06:39 fisyak Exp $
#include "StKFVertex.h"
#include "StKFTrack.h"
#include "TArrayC.h"
#include "TArrayI.h"
#include "TArrayD.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StG2TrackVertexMap.h"

#define __DEBUG__
#if defined(__DEBUG__)
#define PrPP(A,B) if (Debug()) {cout << "StKFVertex::" << (#A) << "\t" << (#B) << " = \t" << (B) << endl;}
#else
#define PrPP(A,B)
#endif
using namespace std;
Int_t StKFVertex::_debug = 0;
Int_t StKFVertex::fTotalNoVertices = 0;
const Char_t *StKFVertex::GeNames[52] = {
  //   1       2       3      4         5           6       7        8         9          10
  "",
  "gamma"   ,"e+"   ,"e-"  ,"nu"   ,"mu+"   ,"mu-"   ,"pi0"  ,"pi+"   ,"pi-"      ,"K0L",
  "K+"      ,"K-"   ,"N"   ,"P"    ,"Pbar"  ,"K0S"   ,"eta"  ,"Lambda","Sigma+"   ,"Sigma0",
  "S-"      ,"Xi0"  ,"Xi-" ,"Omega","Nbar"  ,"LamBar","SBar-","SBar0" ,"SBar+"    ,"XiBar0",
  "XiBar+"  ,"OmBar","tau+","tau-" ,"D+"    ,"D-"    ,"D0"   ,"Dbar0" ,"Ds+"      ,"Ds-"   ,
  "LambC+"  ,"W+"   ,"W-"  ,"Z0"   ,"H2"    ,"H3"    ,"alpha","geanti","He3"      ,"Cerenk",
  "??????"};
//________________________________________________________________________________
ClassImp(StKFVertex);
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StKFVertex& v) {
  os << Form("%3i with %4i tracks Q:%3i", v.ID(),v.NoTracks(),v.Charge());
  for (Int_t i = 0; i < 3; i++) {
    Double_t errI = (v.GetCovariance(i,i) > 0) ? TMath::Sqrt(v.GetCovariance(i,i)) : -13;
    os << Form("%9.3f +/- %5.3f",v.GetParameter(i),errI);
  }
  if (v.GetNDF() > 0) {
    Double_t prob = TMath::Prob(v.GetChi2(),v.GetNDF());
    os << Form("\tchi2/NDF = %8.2f/%4i",v.GetChi2(),v.GetNDF())
       << Form(" prob = %6.4f",prob);
    Float_t M, dM;
    if (! v.GetMass(M,dM)) {
      if (dM < 1e2)     os << " M = " << Form("%7.3f +/- %5.3f",M,dM);
    }
  }
  Int_t kv = v.IdTruth();
  if (kv > 0) {
    os << Form(" Mc/QA/t:%4i/%3i/%6.0f xyz: %8.3f%8.3f%8.3f m:%4i %6s",kv, v.QaTruth(),
	       v.TimeMc(), v.XyzMc().X(), v.XyzMc().Y(), v.XyzMc().Z(), 
	       v.NoDaughtersMc(),StKFVertex::StKFVertex::GeNames[v.gePidMc()]);
  }
  return os;
}
//________________________________________________________________________________
StKFVertex::StKFVertex(const StKFVertex& vtx) {
  Vertex() = vtx.Vertex();
  fTimeMc  = vtx.TimeMc();
  fXyzMc   = vtx.XyzMc();
  fNoDaughtersMc = vtx.NoDaughtersMc();
  fgePidMc = vtx.gePidMc();
  if (fKFTracks.IsOwner()) fKFTracks.Delete();
  fKFTracks.SetOwner(kFALSE);
  TIter next2(&((StKFVertex *)&vtx)->Tracks(),kIterBackward);
  StKFTrack *t2 = 0;
  while ((t2 = (StKFTrack *) next2())) {
    fKFTracks.Add(t2);
  }
}
//________________________________________________________________________________
void StKFVertex::ResetParticles() {
  Int_t N = NoTracks();
  TIter next(&fKFTracks,kIterBackward);
  StKFTrack *Track = 0;
  while ((Track = (StKFTrack *) next())) {
    KFParticle *particle = (KFParticle *) Track->OrigParticle();
    if (! particle || Track->Weight() <= 0.) {// Track->W() <= 0.
      delete Remove(Track);
    } else {
      particle->SetParentID();
      Track->ResetParticle();
    } 
  }
}
//________________________________________________________________________________
Bool_t StKFVertex::Fit() {
  // Clean up
  ResetParticles();
  CheckBeamConstraint();
  Int_t N = NoTracks();
  if (N < 2) {
    if (Debug()) {
      Print("StKFVertex::Fit fails, N < 2 ");
    }
    return kFALSE;
  }
  if (Debug()) {
    PrintW("StKFVertex::Fit");
  }
  KFParticle **particles = new KFParticle*[N];
  
  TIter next(&fKFTracks,kIterForward);
  StKFTrack *Track = 0;
  Int_t i = 0;
  while ((Track = (StKFTrack *) next())) {
    particles[i] = &(Track->Particle());
    i++;
  }
  PrPP(Fit before,Vertex());
  TArrayC Flag(N);
#if 0 /* removed SetVtxGuess from KFParticle */
  KFVertex temp;
  temp.Clear();
  temp.SetVtxGuess(X(),Y(),Z());
  //  PrPP(Fit before,temp);
  temp.ConstructPrimaryVertex((const KFParticle **) particles, N, 
				  (Bool_t*) Flag.GetArray(),TMath::Sqrt(StAnneling::Chi2Cut()/2));
  //  PrPP(Fit after Fit,temp);
  SetVtxGuess(temp.X(),temp.Y(),temp.Z());
#endif
  ConstructPrimaryVertex((const KFParticle **) particles, N, 
				  (Bool_t*) Flag.GetArray(),TMath::Sqrt(StAnneling::Chi2Cut()/2));

  PrPP(Fit after,Vertex());
  //Check Covariance Matrix
  //  Double_t prob = TMath::Prob(GetChi2(),GetNDF());
  TRSymMatrix CL(3,CovarianceMatrix());
  TRSymMatrix CLI(CL,TRArray::kInvertedA);
  Bool_t fail = ! CLI.IsValid();
  TIter nextI(&fKFTracks,kIterBackward);
  Track = 0;
  i = N-1;
  while ((Track = (StKFTrack *) nextI())) {
    if(fail || ! Flag[i])  delete Remove(Track);
    i--;
  }
  delete [] particles;
  N = NoTracks();
  if (! N) {
    if (Debug()) {
      Print("StKFVertex::Fit fails, N = 0 ");
    }
    return kFALSE;
  }
  SetMc();
  return kTRUE;
}
//________________________________________________________________________________
void StKFVertex::AddTrack(const StKFTrack *track) {
  if (! track) return;
  StKFTrack*  t1 = (StKFTrack*) fKFTracks.FindObject(track);
  if (t1) {
    Int_t k1 = t1->K();  // protect from multiple copies of beam track
    if (! k1) {return;}
    assert(! k1);
  }
  fKFTracks.AddLast((TObject *)track);
}
//________________________________________________________________________________
void StKFVertex::UpdateVertex2TrackChi2() {
  //  Int_t Ntracks = NoTracks();
  PrPP(UpdateVertex2TrackChi2,Vertex());
  if (_debug > 1) PrintW("old Weghts ");
  
  TIter next(&fKFTracks,kIterBackward);
  StKFTrack *Track = 0;
  while ((Track = (StKFTrack *) next())) {
    KFVertex vTmp = Vertex();
    //    PrPP(UpdateVertex2TrackChi2,vTmp);
    StKFTrack &track = *Track;
    //    PrPP(UpdateVertex2TrackChi2,track.Particle());
#if 0 /* -=  is unstable now */
    vTmp -= track.Particle();
#endif
    //    PrPP(UpdateVertex2TrackChi2,vTmp);
    KFParticle *particle = (KFParticle *) track.OrigParticle();
    if (! particle) continue;
    //    PrPP(UpdateVertex2TrackChi2,*particle);
    Double_t chi2il = particle->GetDeviationFromVertex(vTmp);
    if (chi2il < 0 || chi2il > StAnneling::Chi2Cut()) {
      particle->SetParentID();
      delete Remove(Track);
      continue;
    }
    track.SetChi2(chi2il);
    if (particle->Id() > 0 && chi2il < StAnneling::Chi2CutUniq()) {
      //      track.Particle().SetProductionVertex(Vertex());
      particle->SetParentID(Id());
    }
  }
  if (_debug > 1) PrintW("new Weights ");
  return;
}
//________________________________________________________________________________
StKFTrack*   StKFVertex::Remove(KFParticle *particle)    {
  TIter next(&fKFTracks,kIterBackward);
  StKFTrack *Track = 0;
  while ((Track = (StKFTrack *) next())) {
    if (particle == Track->OrigParticle()) return Remove(Track);
  }
  return 0;
}
//________________________________________________________________________________
StKFVertex &StKFVertex::operator+=(StKFVertex &vtx) {
  if (_debug) {
    PrintW("Before Merge 1");
    vtx.PrintW("Before Merge 2");
  }
  //  Int_t N2 = vtx.NoTracks();
  TIter next2(&vtx.Tracks(),kIterBackward);
  StKFTrack *t2 = 0;
  while ((t2 = (StKFTrack *) next2())) {
    Int_t k2 = t2->K();
    TIter next1(&Tracks(),kIterForward);
    StKFTrack *t1 = 0;
    while ((t1 = (StKFTrack *) next1())) {
      Int_t k1 = t1->K();
      if (k1 == k2) {t2 = 0; break;}
    }
    if (t2) fKFTracks.AddLast(new StKFTrack(*t2));
  }
  if (_debug > 1) {
    PrintW("After Merge 1");
    vtx.PrintW("After Merge 2");
  }
  return *this;
}
//________________________________________________________________________________
StKFVertex &StKFVertex::operator=(const StKFVertex &vtx) {
  if (this != &vtx) {
    if (_debug > 1) {
      PrintW("StKFVertex::operator= before");
    }
    Vertex() = vtx.Vertex();
    fTimeMc = vtx.TimeMc();
    fXyzMc  = vtx.XyzMc();
    fNoDaughtersMc = vtx.NoDaughtersMc();
    fgePidMc = vtx.gePidMc();
    fKFTracks.Clear();
    if (_debug > 1) {
      PrintW("StKFVertex::operator= after Clear");
    }
    fKFTracks.SetOwner(kTRUE);
    TIter next2(&((StKFVertex *)&vtx)->Tracks());
    StKFTrack *t2 = 0;
    while ((t2 = (StKFTrack *) next2())) {
      if (_debug > 1) {
	cout << "t2 " << *t2 << endl;
      }
      StKFTrack *t1 = new StKFTrack(*t2);
      if (_debug > 1) {
	cout << "t1 " << *t1 << endl;
      }
      fKFTracks.Add(t1);
    }
    if (_debug > 1) {
      PrintW("StKFVertex::operator= after assignment");
    }
  }
  return *this;
}
//________________________________________________________________________________
StKFVertex &StKFVertex::operator=(const KFVertex &vtx) {
  if (_debug > 1) {
    PrintW("StKFVertex::operator= before");
  }
  Vertex() = vtx;
  return *this;
}
//________________________________________________________________________________
void StKFVertex::PrintW(Option_t *option) const {
  Int_t N = NoTracks();
  cout << Form("V[%5i] with %5i tracks\t",Id(),N);
  Print(option);
  if (_debug > 2) {
    cout   << Form("     i    k    Weight        W        Z       chi2") << endl;
    TIter next(&fKFTracks,kIterForward);
    StKFTrack *Track = 0;
    Int_t i = 0;
    while ((Track = (StKFTrack *) next())) {
      cout << Form("%6i",i) << *Track << endl;
      i++;
    }
  }
}
//________________________________________________________________________________
void StKFVertex::CheckBeamConstraint() {
  Int_t N = NoTracks();
  StKFTrack*  tbeam = 0;
  //  if (_debug > 0)  PrintW("CheckBeamConstraint");
  TIter next(&fKFTracks,kIterForward);
  StKFTrack *t = 0;
  while ((t = (StKFTrack *) next())) {
    if (! t) continue;
    const KFParticle *po = t->OrigParticle();
    if (! po) continue;
    //    PrPP(CheckBeamConstraint,*po);
    Int_t kg = po->Id();
    if (kg < 1) {
      assert(! tbeam);
      tbeam = (StKFTrack*) t;
    }
  }
  if (! tbeam) {
    SetBeamConstraintOff(); 
  } else {
    St_vertexSeedC *vSeed = St_vertexSeedC::instance();
    assert(vSeed);
    Double_t z = Z();
    Double_t x = vSeed->x0() + vSeed->dxdz()*z;
    Double_t y = vSeed->y0() + vSeed->dydz()*z;
    KFParticle &particle = tbeam->Particle();
    particle.X() = x;
    particle.Y() = y;
    particle.Z() = z;
    PrPP(CheckBeamConstraint beam,particle);
  }
}
//________________________________________________________________________________
Double_t StKFVertex::Chi2AtVx() {
  Double_t chi2Vx = 0;
  TIter next(&fKFTracks,kIterForward);
  StKFTrack *t = 0;
  while ((t = (StKFTrack *) next())) {
    if (t->W() <= 0) continue;
    // W.Waltenberger, R.Fruhwirth, P. Vanlaer, "Adaptive vertex fitting"
    // J.Phys. G: Nucl. Part. Phys. 34 (2007) N343-N356. Eq.(9)
    Double_t chi2  = t->Chi2();
    Double_t chi2C =  StAnneling::Chi2Cut();
    Double_t T = StAnneling::Temperature();
#if 0
    Double_t dChi2 = chi2/2  -
      T*TMath::Log((TMath::Exp(chi2/(2*T)) + TMath::Exp(chi2C/(2*T)))) +
      T*TMath::Log( 1.                     + TMath::Exp(chi2C/(2*T)));
#else
    //    Double_t dChi2 =  chi2/2 + TMath::Log(track.Weight() + StAnneling::Weight());
    Double_t dChi2 = chi2/2  - T*TMath::Log(
					    (TMath::Exp(-chi2/(2)) + TMath::Exp(-chi2C/(2*T)))/
					    (                   1. + TMath::Exp(-chi2C/(2*T)))
					    );
#endif
    chi2Vx += dChi2;
  }
  return chi2Vx;  
}
//________________________________________________________________________________
void StKFVertex::SetMc() {
  if (! StG2TrackVertexMap::instance()) return;
  if (! StG2TrackVertexMap::instance()->Tracks() || ! StG2TrackVertexMap::instance()->Vertices()) return;
  Int_t NoMuMcVertex = StG2TrackVertexMap::instance()->Vertices()->GetNRows();
  Int_t NoMuMcTracks = StG2TrackVertexMap::instance()->Tracks()->GetNRows();
  const g2t_vertex_st *mcVtx = StG2TrackVertexMap::instance()->Vertices()->GetTable();
  const g2t_track_st  *mcTrk = StG2TrackVertexMap::instance()->Tracks()->GetTable();
  // Set IdTruth
  std::map< Int_t,Float_t> idTruths;
  Int_t IdVx = 0;
  Int_t qa = 0;
  TIter nextlT(&Tracks(),kIterForward);
  StKFTrack *TrackL = 0;
  while ((TrackL = (StKFTrack *) nextlT())) {
    const KFParticle *particle = TrackL->OrigParticle();
    Int_t IdTk = particle->IdTruth();
    if (!IdTk) continue;
    IdVx = particle->IdParentMcVx();
    if (IdVx <= 0) continue;
    qa = particle->QaTruth(); if (!qa) qa = 1;
    idTruths[IdVx] += qa;
  }
  if (idTruths.empty()) return;		//no simu info
  Int_t vxBest = 0; 
  Float_t qaBest = 0, qaSum = 0;
  for (std::map< Int_t,Float_t>::const_iterator it=idTruths.begin(); it!=idTruths.end(); ++it) {
    qaSum += (*it).second;
    if ((*it).second < qaBest) continue;
    vxBest = (*it).first; qaBest = (*it).second;
  }
  if (vxBest <= 0) return;
  Int_t avgQua = 100*qaBest/(qaSum+1e-10)+0.5;
  SetIdTruth(vxBest,avgQua);
  Int_t IdParentTk = StG2TrackVertexMap::instance()->IdParentTrack(vxBest);
  Vertex().SetIdParentMcVx(IdParentTk);
  
  Int_t kv = IdTruth() - 1;
  if (kv >= 0 && kv < NoMuMcVertex) {
    Int_t kvp = mcVtx[kv].parent_p - 1;
    Int_t gePid = 0;
    if (kvp >= 0 &&  kvp < NoMuMcTracks) gePid = mcTrk[kvp].ge_pid;
    fTimeMc = 1e9*mcVtx[kv].ge_tof;
    fXyzMc = TVector3(mcVtx[kv].ge_x);
    fNoDaughtersMc = mcVtx[kv].n_daughter;
    fgePidMc = StKFTrack::CorrectGePid(gePid);
  }
}
//________________________________________________________________________________
Int_t StKFVertex::NoTracks() const {
  if (fKFTracks.IsEmpty()) return 0;
  TIter next(&fKFTracks);
  Int_t N = 0;
  while ((next())) {
    N++;
  }
  return N;
}
#undef PrPP
// $Log: StKFVertex.cxx,v $
// Revision 2.5  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.5  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 1.2  2014/01/14 14:49:17  fisyak
// Freeze
//
// Revision 1.1.1.1  2013/08/13 22:20:41  fisyak
// Save m version
//
// Revision 2.3  2013/04/08 19:21:41  fisyak
// Adjust for new KFParticle
//
// Revision 2.2  2012/06/11 15:33:41  fisyak
// std namespace
//
// Revision 2.1  2012/05/07 14:56:14  fisyak
// Add StKFVertexMaker
//
// Revision 1.3  2012/03/29 23:35:47  fisyak
// Fix problem with multiple beam tracks
//
// Revision 1.2  2012/02/07 19:38:26  fisyak
// Repackage
//
