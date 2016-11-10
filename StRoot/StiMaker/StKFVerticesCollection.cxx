// $Id: StKFVerticesCollection.cxx,v 2.5 2015/12/20 01:06:39 fisyak Exp $
#include <map>
#include <algorithm>
#include "StKFVerticesCollection.h"
#include "TArrayI.h"
#include "TArrayD.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
#include "TPolyMarker.h"
#include "TList.h"
#include "StDetectorDbMaker/St_vertexSeedC.h"
#include "StG2TrackVertexMap.h"
using namespace std;
ClassImp(StKFVerticesCollection);
Double_t StKFVerticesCollection::fgVxPenaltyFactor = 1000;
#define PrPP(A,B) if (StKFVertex::Debug()) {cout << "StKFVerticesCollection::" << (#A) << "\t" << (#B) << " = \t" << (B) << endl;}
//________________________________________________________________________________
StKFVerticesCollection::StKFVerticesCollection(Int_t NoPeaks, Double_t *zOfPeaks, Double_t sigmaXY, Double_t sigmaZ, St_vertexSeedC *vSeed) {
  fVertices.SetOwner(kTRUE);
  for (Int_t peak = 0; peak < NoPeaks; peak++) {
    Double_t x = 0;
    Double_t y = 0;
    if (vSeed) {
      x = vSeed->x0() + vSeed->dxdz()*zOfPeaks[peak];
      y = vSeed->y0() + vSeed->dydz()*zOfPeaks[peak];
    }
    AddVertex(x,y, zOfPeaks[peak], sigmaXY, sigmaZ);
  }
}
//________________________________________________________________________________
void StKFVerticesCollection::AddVertex(Double_t x, Double_t y, Double_t z, Double_t sigmaXY, Double_t sigmaZ) {
  StKFVertex *vtx = new StKFVertex();
  vtx->Vertex().SetBeamConstraint(x, y, z, sigmaXY, sigmaXY, sigmaZ);
  vtx->Vertex().SetBeamConstraintOff();
  fVertices.AddLast(vtx);
#if 0 /* removed SetVtxGuess from KFParticle */
  vtx->Vertex().SetVtxGuess(x,y,z);
#endif
}
//________________________________________________________________________________
void StKFVerticesCollection::operator +=(StKFVerticesCollection &col) {
  TIter next(col.Vertices(),kIterBackward);
  TObject *o = 0;
  while ((o = next())) fVertices.AddLast(col.Vertices()->Remove(o));
}
//________________________________________________________________________________
void StKFVerticesCollection::SetMc() {
  TIter next(&fVertices);
  StKFVertex *V;
  while ((V = (StKFVertex *)  next())) {
    V->SetMc();
  }
}
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StKFVerticesCollection& vc) {
  Int_t l = 0;
  TIter next(vc.Vertices());
  StKFVertex *V = 0;
  while ((V = (StKFVertex *)  next())) {
    os << Form("Vtx: %3i",l) << *V << endl;
    l++;
  }
  return os;
}
//________________________________________________________________________________
void StKFVerticesCollection::DoTrack2VertexAssociation(const TObjArray &particles, Int_t *Parents) {
  Int_t Ntracks = particles.GetEntriesFast();
  TArrayI Idx(Ntracks);
  TArrayD Chi2s(Ntracks);
  TIter next(&fVertices,kIterBackward);
  StKFVertex *vtx;
  while ((vtx = (StKFVertex *)  next())) {
    if (! vtx) continue;
    //    if (! vtx->Vertex()) continue;
    //    vtx->PrintW();
    KFParticle *particle = 0;
    for (Int_t k = 0; k < Ntracks; k++) {
      Chi2s[k] = 1e10;
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (Parents[k]) continue;
      Double_t chi2il = particle->GetDeviationFromVertex(vtx->Vertex());
      if (chi2il < 0) continue;
      Chi2s[k] = chi2il;
    }
    vtx->Clear();    
    TMath::Sort(Ntracks,Chi2s.GetArray(),Idx.GetArray(),0);
    for (Int_t j = 0; j < Ntracks; j++) {
      Int_t k = Idx[j];
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (Chi2s[k] > StAnneling::Chi2Cut()) break;
      StKFTrack *track = new StKFTrack(particle,Chi2s[k]);
      vtx->AddTrack(track);
    }
    if (vtx->NoTracks() < 2) {
      delete fVertices.Remove(vtx);
      continue;
    }
    if (StKFVertex::Debug()) vtx->PrintW("DoTrack2VertexAssociation ");
    //    vtx->PrintW();
  }
  if (! IsEmpty()) UpdateWeights();
  return;
}
//________________________________________________________________________________
void StKFVerticesCollection::UpdateStVertexTrackAssociation() {
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::UpdateStVertexTrackAssociation\t" << *this << endl;
  }
  TIter next(&fVertices,kIterForward);
  StKFVertex *vtx;
  while ((vtx = (StKFVertex *)  next())) {
    vtx->UpdateVertex2TrackChi2(); 
    if (StKFVertex::Debug()) {
      cout << "StKFVerticesCollection::UpdateStVertexTrackAssociation" << *vtx << endl;
    }
  }
  return;
}
//________________________________________________________________________________
void StKFVerticesCollection::MergeDuplicatedVertices() {
  if (NoVertices() < 2) return;
  // Check that vertices are the same
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::MergeDuplicatedVertices\t" << endl << *this;
  }
  for (Int_t l = fVertices.GetSize() - 1; l >= 0; l--) {
    StKFVertex *vtxl = (StKFVertex *) fVertices.At(l);
    if (! vtxl || ! vtxl->NoTracks()) {delete fVertices.Remove(vtxl);}
  }
  for (Int_t l = 0;; l++) {
    if (l >= fVertices.GetSize()) break;
    StKFVertex *vtxl = (StKFVertex *) fVertices.At(l);
    TRVector vL(3,vtxl->Vertex().Parameters());
    TRSymMatrix CL(3,vtxl->Vertex().CovarianceMatrix());
    Int_t NoVx = fVertices.GetSize();
    for (Int_t m = NoVx - 1; m > l; m--) {
      StKFVertex *vtxm = (StKFVertex *) fVertices.At(m);
      if (vtxl == vtxm) continue;
      TRVector vM(3,vtxm->Vertex().Parameters());
      vM -= vL;
      if (vM.Mag() > 5.) continue;
      TRSymMatrix CM(3,vtxm->Vertex().CovarianceMatrix());
      CM += CL;
      CM[0] += 0.0001; // 100 mkm tolerance
      CM[2] += 0.0001;
      CM[5] += 0.0001;
      TRSymMatrix G(CM,TRArray::kInvertedA);
      if (! G.IsValid()) continue;
      Double_t chi2 = G.Product(vM,TRArray::kATxSxA);
      Double_t prob = TMath::Prob(chi2,3);
      if (prob > 1e-4) {
	if (StKFVertex::Debug()) {
	  cout << "StKFVerticesCollection::MergeDuplicatedVertices prob = " << prob << endl;
	  vtxl->PrintW("Merge Vertex l");
	  vtxm->PrintW("and   Vertex m");
	}
#if 0
	StKFVertex *temp  = new StKFVertex(*vtxl); PrPP(copy vtxl,*temp);
	StKFVertex *tempM = new StKFVertex(*vtxm); PrPP(copy vtxm,*tempM);
	*temp += *tempM;                           PrPP(add  vtxm,*temp);
	if (StKFVertex::Debug()) {
	  temp->PrintW("Temp");
	}
	if (temp->Fit()) {
	  if (StKFVertex::Debug()) {
	    temp->PrintW("Merged temp vertex"); 
	  }
	  //	  *vtxl = *temp;
	  TObject **ref  = fVertices.GetObjectRef(vtxl);
	  delete vtxl;
	  vtxl = temp;
	  *ref = vtxl;
	  delete fVertices.Remove(vtxm); nextm.Reset(); m = -1;
	  if (StKFVertex::Debug()) {
	    vtxl->PrintW("Merged vertex l");
	  }
	}
	delete temp;
	delete tempM;
#else
	*vtxl += *vtxm;
	delete fVertices.Remove(vtxm); 
	if (StKFVertex::Debug()) {
	  vtxl->PrintW("Merged vertex l");
	}
#endif
      }
    }
  }
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::UpdateWeights() {
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::UpdateWeights\t" << *this << endl;
  }
  std::multimap<const KFParticle*,Map_t> Particle2Track;
  std::pair< std::multimap<const KFParticle*,Map_t>::iterator, std::multimap<const KFParticle*,Map_t>::iterator> ret;
  std::multimap<const KFParticle*,Map_t>::iterator it;
  std::vector<const KFParticle *>     ParticleM;
  std::vector<const KFParticle *>::iterator iter;
  TIter nextL(&fVertices,kIterBackward);
  StKFVertex *vtxl = 0;
  //  Int_t k = 0;
  while ((vtxl = (StKFVertex *)  nextL())) {
    if (StKFVertex::Debug() > 1) vtxl->PrintW("Original Weights ");
    // recalculate weights
    TIter nextlT(&vtxl->Tracks(),kIterForward);
    StKFTrack *TrackL = 0;
    while ((TrackL = (StKFTrack *) nextlT())) {
      Particle2Track.insert( std::pair<const KFParticle*,Map_t>(TrackL->OrigParticle(),Map_t(vtxl,TrackL,TrackL->Weight())));
      ParticleM.push_back(TrackL->OrigParticle());
    }
  }
  std::sort(ParticleM.begin(), ParticleM.end());
  iter = std::unique(ParticleM.begin(), ParticleM.end());
  ParticleM.resize( std::distance(ParticleM.begin(),iter) );
  iter = ParticleM.begin();
  while (iter != ParticleM.end()) {
    const KFParticle* part = *iter;
    ret = Particle2Track.equal_range(part);
    Double_t Dominator = TMath::Exp(-StAnneling::Chi2Cut()/(2*StAnneling::Temperature()));
    for (it = ret.first; it!=ret.second; ++it) {
      Dominator += it->second.Weight;
    }
    for (it = ret.first; it!=ret.second; ++it) {
      StKFTrack *TrackL = it->second.track;
      TrackL->NormW(Dominator);
    }
    iter++;
  }
  Double_t chi2Total = 0;
  nextL.Reset();
  while ((vtxl = (StKFVertex *)  nextL())) {
    if (StKFVertex::Debug() > 1) vtxl->PrintW("Updated Weights ");
    chi2Total += fgVxPenaltyFactor + vtxl->Chi2AtVx();
  }
  return chi2Total;
}
//________________________________________________________________________________
void StKFVerticesCollection::UniqueTracks2VertexAssociation(){
  // remove 2-prongs if there is no beam as ill defined vertex 
  // Make track associated with only vertex (by maximum weight to the vertex) 
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::UniqueTracks2VertexAssociation\t" << endl << *this << endl;
  }
  std::multimap<const KFParticle*,Map_t> Particle2Track;
  std::vector<const KFParticle *>     ParticleM;
  TIter nextL(&fVertices,kIterBackward);
  StKFVertex *vtxl = 0;
#if 0
  // Remove 2 prong vertex if no beam line
  while ((vtxl = (StKFVertex *)  nextL())) {
    // recalculate weights
    TIter nextlT(&vtxl->Tracks(),kIterForward);
    StKFTrack *TrackL = 0;
    Int_t NoTracks = 0;
    while ((TrackL = (StKFTrack *) nextlT())) {
      NoTracks++;
      if (! TrackL->Id()) NoTracks++; // beam line
    }
    if (NoTracks <= 2) {
      nextlT.Reset();
      while ((TrackL = (StKFTrack *) nextlT())) {
	delete vtxl->Tracks().Remove(TrackL);
      }
      delete fVertices.Remove(vtxl);
    }
  }
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::UniqueTracks2VertexAssociation. After clean up V2\t" << endl << *this << endl;
  }
  nextL.Reset();
#endif
  // Select the best track to vertex association (no compition between 2-prong vertices)
  while ((vtxl = (StKFVertex *)  nextL())) {
    // recalculate weights
    TIter nextlT(&vtxl->Tracks(),kIterForward);
    StKFTrack *TrackL = 0;
    while ((TrackL = (StKFTrack *) nextlT())) {
      Particle2Track.insert( std::pair<const KFParticle*,Map_t>(TrackL->OrigParticle(),Map_t(vtxl,TrackL,TrackL->W())));
      ParticleM.push_back(TrackL->OrigParticle());
    }
  }
  std::sort(ParticleM.begin(), ParticleM.end());
  std::vector<const KFParticle *>::iterator iter;
  iter = std::unique(ParticleM.begin(), ParticleM.end());
  ParticleM.resize( std::distance(ParticleM.begin(),iter) );
  for (iter = ParticleM.begin(); iter != ParticleM.end(); ++iter) {
    const KFParticle* part = *iter;
    // Clean up all non beam tracks
    if (! part->Id()) continue;
    if (Particle2Track.count(part) <= 1) continue;
    std::pair< std::multimap<const KFParticle*,Map_t>::iterator, std::multimap<const KFParticle*,Map_t>::iterator> ret;
    ret = Particle2Track.equal_range(part);
    Double_t Wmax = -1;
    StKFVertex *vertMax = 0;
    std::multimap<const KFParticle*,Map_t>::iterator itMax;
    for (std::multimap<const KFParticle*,Map_t>::iterator it = ret.first; it!=ret.second; ++it) {
      if (it->second.Weight > Wmax) {
	itMax = it;
	Wmax = it->second.Weight;
	vertMax = it->second.vert;
      }
    }
    for (std::multimap<const KFParticle*,Map_t>::iterator it = ret.first; it!=ret.second; ++it) {
      if (it == itMax) continue;
      StKFVertex *vert = it->second.vert;
      // No compition between 2-prong vertices
      if (vert->NoTracks() == 2 && vertMax->NoTracks() == 2) continue;
      StKFTrack  *track = it->second.track;
      delete vert->Tracks().Remove(track);
    }
  }
  nextL.Reset();
  while ((vtxl = (StKFVertex *)  nextL())) {
    if (vtxl->Tracks().IsEmpty()) {delete fVertices.Remove(vtxl); continue;}
    vtxl->SetMc();
  }
  //  SetParents();
  nextL.Reset();
  while ((vtxl = (StKFVertex *)  nextL())) {
    StKFTrack *track;
    vtxl->CleanDaughtersId();
    TIter nextT(&vtxl->Tracks(),kIterForward);
    while ((track = (StKFTrack *) nextT())) {
      if (track->Chi2() < StAnneling::Chi2CutUniq()) {
	UInt_t k = track->K();
	if (k) {
	  track->SetParent(vtxl->Id());
	  vtxl->AddDaughterId(track->Id());
	}
	if (! vtxl->NoTracks()) break;
	continue;
      }
    }
  }
  SetMc();
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::Fit(Int_t marker, TCanvas *c1, TH1 *VertexZPlot) {
  MergeDuplicatedVertices();
  // Primary Vertex fit
  Double_t chi2Total = 1e10;
  if (fVertices.IsEmpty()) return chi2Total; 
  chi2Total = 0;
  TIter next(&fVertices);
  StKFVertex *vtx;
  while ((vtx = (StKFVertex *)  next())) {
    if (! vtx->Fit() || vtx->Vertex().GetNDF() < 1 || vtx->NoTracks() < 2) {
      PrPP(Fit Failed, *vtx);
      delete fVertices.Remove(vtx);
      continue;
    } else {
      PrPP(Fit Passed, *vtx);
    }
  }
  UpdateStVertexTrackAssociation();
  chi2Total = UpdateWeights();
  //
  if (StKFVertex::Debug()) {
    cout << "chi2Total = " << chi2Total 
	 << " at Temperature " << StAnneling::Temperature() 
	 << " and Log(Temperature) " << TMath::Log(StAnneling::Temperature()) 
	 << " no. vertices " << NoVertices()
	 << endl;
  }
  if (VertexZPlot) {
    Double_t ymax = VertexZPlot->GetMaximum();
    next.Reset();
    while ((vtx = (StKFVertex *)  next())) {
      Double_t X = vtx->Vertex().GetParameter(2);
      Double_t Y = vtx->NoTracks();
      if (Y > ymax) ymax = Y;
      TPolyMarker * pm = new TPolyMarker(1, &X, &Y);
      VertexZPlot->GetListOfFunctions()->Add(pm);
      pm->SetMarkerColor(TMath::Log(StAnneling::Temperature())+2);
      Int_t m = 22;
      if (marker) {
	m = marker;
	pm->SetMarkerColor(4);
	if (StKFVertex::Debug()) vtx->PrintW();
      }
      pm->SetMarkerStyle(m);
      pm->SetMarkerSize(2);
      //	chi2NDF->Fill(TMath::Log10(vtx->NoTracks()),vtx->Vertex().GetChi2()/vtx->Vertex().GetNDF());
    }
    if (c1) {
      c1->cd();
      VertexZPlot->SetMaximum(ymax);
      VertexZPlot->Draw("same");
      c1->Update();
    }
  }
  return chi2Total;
}
//________________________________________________________________________________
void  StKFVerticesCollection::SetParents(Int_t *parents) const {
  assert(parents);
  if (StKFVertex::Debug()) {
    cout << "StKFVerticesCollection::SetParents arrray\t" << *this << endl;
  }
  TIter next(&fVertices,kIterForward);
  StKFVertex *vtx;
  StKFTrack *track;
  while ((vtx = (StKFVertex *)  next())) {
    TIter nextT(&vtx->Tracks(),kIterForward);
    vtx->CleanDaughtersId();
    while ((track = (StKFTrack *) nextT())) {
      if (track->Chi2() < StAnneling::Chi2CutUniq()) {
	UInt_t k = track->K();
	if (k) {
	  if (parents) parents[k] = vtx->Id();
	  else        {
	    track->SetParent(vtx->Id());
	    vtx->AddDaughterId(track->Id());
	  }
	}
      }
    }
  }
}
//________________________________________________________________________________
Int_t StKFVerticesCollection::NoVertices() const {
  TIter next(&fVertices);
  Int_t N = 0;
  while ((next())) {
    N++;
  }
  return N;
}
//________________________________________________________________________________
// $Log: StKFVerticesCollection.cxx,v $
// Revision 2.5  2015/12/20 01:06:39  fisyak
// Merge
//
// Revision 2.5  2015/01/05 21:04:31  fisyak
// Add access to TMVA ranking
//
// Revision 1.3  2014/01/14 14:49:18  fisyak
// Freeze
//
// Revision 1.2  2013/10/16 13:19:15  fisyak
// Add beam line position to PV guess, add Z error in beam track, relax requirements on vertex seed
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
// Revision 1.5  2012/03/29 23:35:47  fisyak
// Fix problem with multiple beam tracks
//
// Revision 1.4  2012/03/26 23:42:35  fisyak
// Add beam constrain
//
// Revision 1.3  2012/02/07 19:38:26  fisyak
// Repackage
//
