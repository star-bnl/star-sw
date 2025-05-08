// $Id: StKFVertex.cxx,v 2.5 2018/04/10 11:32:09 smirnovd Exp $
#include "StKFVertex.h"
#include "StKFTrack.h"
#include "TArrayC.h"

#define __DEBUG__
#if defined(__DEBUG__)
#define PrPP(A,B) if (Debug()) {cout << "StKFVertex::" << (#A) << "\t" << (#B) << " = \t" << (B) << endl;}
#else
#define PrPP(A,B)
#endif
using namespace std;
Int_t StKFVertex::_debug = 0;
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
ostream&  operator<<(ostream& os,  const StKFVertex& v) {
  Int_t iWest = v.MultW();
  Int_t iEast = v.MultE();
  os << Form(" with %4i tracks Q:%3i W/E = %3i/%3i",v.NoTracks(),v.Charge(),iWest,iEast);
  for (Int_t i = 0; i < 3; i++) {
    os << Form("%9.3f +/- %5.3f",v.Vertex().GetParameter(i),TMath::Sqrt(v.Vertex().GetCovariance(i,i)));
  }
  Double_t prob = TMath::Prob(v.Vertex().GetChi2(),v.Vertex().GetNDF());
  os << Form("\tchi2/NDF = %8.2f/%4i",v.Vertex().GetChi2(),v.Vertex().GetNDF())
     << Form(" prob = %6.4f",prob);
  Double_t M, dM;
  if (! v.Vertex().GetMass(M,dM)) {
    os << " M = " << Form("%7.3f +/- %5.3f",M,dM);
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
void StKFVertex::Fit() {
  Compress();
  Int_t N = NoTracks();
  if (! N) return;
  KFParticle **particles = new KFParticle*[N];
  for (Int_t i = 0; i < N; i++) {
    // Track(i)->Reset(); 
    particles[i] = &(Track(i)->Particle());
  }
  TArrayC Flag(N);
  Vertex().ConstructPrimaryVertex((const KFParticle **) particles, N, 
				  (Bool_t*) Flag.GetArray(),TMath::Sqrt(StAnneling::Chi2Cut()/2));
  //Check Covariance Matrix
  //  Double_t prob = TMath::Prob(Vertex().GetChi2(),Vertex().GetNDF());
  TRSymMatrix CL(3,Vertex().CovarianceMatrix());
  if (CL[0] <= 0 || CL[2] <= 0 || CL[5] <= 0) {
    for (Int_t i = N-1; i >= 0; i--)  delete Remove(i);
  } else {
    for (Int_t i = N-1; i >= 0; i--) if (! Flag[i]) delete Remove(i);
  }
  delete [] particles;
  Compress();
  N = NoTracks();
  // Assign MC and RC
  struct vertexPing {
    Int_t  Id;
    Int_t nPings;
  };
  static vertexPing candidates[20];
  memset(candidates,0,sizeof(candidates));
  Int_t NC = 0;
  for (Int_t i = 0; i < N; i++) {
    const StKFTrack *pTrack = Track(i);
    if (! pTrack) continue;
    Int_t IdVx = pTrack->Particle().IdParentVx();
    if (IdVx <= 0) continue;
    Int_t J = -1;
    for (Int_t j = 0; j < NC; j++) if (candidates[j].Id == IdVx) {J = j; break;}
    if (J < 0) {J = NC; if (NC < 18) NC++;}
    candidates[J].Id = IdVx;
    candidates[J].nPings++;
  }
  Int_t dominant = -1;
  Int_t J = -1;
  for (Int_t j = 0; j < NC; j++) if (candidates[j].nPings > dominant) {dominant = candidates[j].nPings; J = j;}
  if (J > -1) {
    Int_t Id = candidates[J].Id;
    Int_t QA      = (100*dominant)/N;
    SetIdTruth(Id,QA);
  }
  for (Int_t i = 0; i < N; i++) {
    Track(i)->Particle().SetProductionVertex(Vertex());
  }
}
//________________________________________________________________________________
void StKFVertex::AddTrack(const StKFTrack *track) {
  Int_t k2 = track->K()%100000;
  Int_t N1 = NoTracks();
  for (Int_t j = 0; j < N1; j++) {// protect from multiple copies of beam track
    StKFTrack*  t1 = Track(j);
    Int_t k1 = t1->K()%100000;
    if (k1 == k2) {
      PrintW("AddTrack");
      assert(0);
    }
  }
  fKFTracks.AddLast((TObject *)track);
}
//________________________________________________________________________________
Double_t StKFVertex::UpdateVertex2TrackChi2() {
  Int_t Ntracks = NoTracks();
  Double_t chi2Vx = 0;
  PrPP(UpdateVertex2TrackChi2,fVertex);
  if (_debug) PrintW("old Weghts ");
  for (Int_t k = Ntracks - 1; k >= 0; k--) {
    KFVertex vTmp = fVertex;
    //    PrPP(UpdateVertex2TrackChi2,vTmp);
    StKFTrack &track = *Track(k);
    //    PrPP(UpdateVertex2TrackChi2,track.Particle());
    vTmp -= track.Particle();
    //    PrPP(UpdateVertex2TrackChi2,vTmp);
    KFParticle *particle = track.OrigParticle();
    if (! particle) continue;
    //    PrPP(UpdateVertex2TrackChi2,*particle);
    Double_t chi2il = particle->GetDeviationFromVertex(vTmp);
    chi2il *= 2*chi2il;
    if (chi2il > StAnneling::Chi2Cut()) {
      Remove(k);
      continue;
    }
    track.SetChi2(chi2il);
    chi2Vx += track.Chi2()/2 + TMath::Log(track.Weight() + StAnneling::Weight());
  }
  Compress();
  if (_debug) PrintW("new Weights ");
  return chi2Vx;
}
//________________________________________________________________________________
StKFTrack*   StKFVertex::Remove(KFParticle *particle)    {
  Int_t N = NoTracks();
  for (Int_t k = 0; k < N; k++) if (particle == Track(k)->OrigParticle()) return Remove(k);
  return 0;
}
//________________________________________________________________________________
Int_t        StKFVertex::MultWE(Int_t k) const {
  Int_t N = NoTracks();
  Int_t iWE = 0;
  for (Int_t i = 0; i < N; i++) {
    const StKFTrack*  t = Track(i);
    if (t) {
      Int_t id = (t->OrigParticle()->GetID()/100000)%10;
      if (id == k) iWE++;
    }
  }
  return iWE;
}
//________________________________________________________________________________
Int_t StKFVertex::Q() const {
  Int_t iQ = 0;
  Int_t N = NoTracks();
  for (Int_t i = 0; i < N; i++) {
    const StKFTrack*  t = Track(i);
    if (t) {iQ += t->OrigParticle()->GetQ();}
  }
  return iQ;
}
//________________________________________________________________________________
void StKFVertex::operator +=(StKFVertex &vtx) {
  if (_debug) {
    PrintW("Before Merge 1");
    vtx.PrintW("Before Merge 2");
  }
  Int_t N2 = vtx.NoTracks();
  for (Int_t i = N2-1; i >= 0; i--) {
    StKFTrack*  t2  = (StKFTrack* ) vtx.Remove(i); 
    Int_t k2 = t2->K()%100000;
    Int_t N1 = NoTracks();
    for (Int_t j = 0; j < N1; j++) {// protect from multiple copies of beam track
      StKFTrack*  t1 = Track(j);
      Int_t k1 = t1->K()%100000;
      if (k1 == k2) {SafeDelete(t2); break;}
    }
    if (t2) fKFTracks.AddLast(t2);
  }
  vtx.Compress();
  if (_debug) {
    PrintW("After Merge 1");
    vtx.PrintW("After Merge 2");
  }
}
//________________________________________________________________________________
void StKFVertex::PrintW(Option_t *option) const {
  Int_t N = NoTracks();
  cout << Form("Vertex %5i with %5i tracks\t",fID,N);
  Print(option);
  cout   << Form("     i    k    Weight        W        Z       chi2") << endl;
  for (Int_t i = 0; i < N; i++) {
    const StKFTrack*  t = Track(i);
    cout << Form("%6i",i) << *t << endl;
  }
}
//________________________________________________________________________________
void StKFVertex::SetMc(Float_t time, Float_t x, Float_t y, Float_t z, Int_t NoDaughters, Int_t gePid) {
  fTimeMc = 1e9*time;
  fXyzMc = TVector3(x,y,z);
  fNoDaughtersMc = NoDaughters;
  fgePidMc = StKFTrack::CorrectGePid(gePid);
}
#undef PrPP
// $Log: StKFVertex.cxx,v $
// Revision 2.5  2018/04/10 11:32:09  smirnovd
// Minor corrections across multiple files
//
// - Remove ClassImp macro
// - Change white space
// - Correct windows newlines to unix
// - Remove unused debugging
// - Correct StTpcRTSHitMaker header guard
// - Remove unused preprocessor directives in StiCA
// - Minor changes in status and debug print out
// - Remove using std namespace from StiKalmanTrackFinder
// - Remove includes for unused headers
//
// Revision 2.4  2013/04/10 22:14:20  fisyak
// Roll back to version 04/04/2013
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
