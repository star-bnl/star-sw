// $Id: StKFVerticesCollection.cxx,v 2.5 2018/04/10 11:32:10 smirnovd Exp $
#include "StKFVerticesCollection.h"
#include "TArrayI.h"
#include "TArrayD.h"
#include "TRSymMatrix.h"
#include "TRVector.h"
#include "TPolyMarker.h"
#include "TList.h"
using namespace std;
Double_t StKFVerticesCollection::fgVxPenaltyFactor = 1000;
//________________________________________________________________________________
StKFVerticesCollection::StKFVerticesCollection(Int_t NoPeaks, Double_t *zOfPeaks, Double_t sigmaXY, Double_t sigmaZ) : 
  fVertices(NoPeaks,0) {
  fVertices.SetOwner(kTRUE);
  for (Int_t peak = 0; peak < NoPeaks; peak++) {
    AddVertex(0.,0., zOfPeaks[peak], sigmaXY, sigmaZ);
  }
}
//________________________________________________________________________________
void StKFVerticesCollection::AddVertex(Double_t x, Double_t y, Double_t z, Double_t sigmaXY, Double_t sigmaZ) {
  StKFVertex *vtx = new StKFVertex(fVertices.GetEntriesFast() + 1);
  vtx->Vertex().SetBeamConstraint(x, y, z, sigmaXY, sigmaXY, sigmaZ);
  fVertices.AddLast(vtx);
}
//________________________________________________________________________________
void StKFVerticesCollection::operator +=(StKFVerticesCollection &col) {
  Int_t N2 = col.NoVertices();
  for (Int_t i = N2-1; i >= 0; i--) {
    fVertices.AddLast(col.Remove(i));
  }
  col.Compress();
}
//________________________________________________________________________________
void StKFVerticesCollection::SetMc(Int_t NoMuMcVertex, Int_t NoMuMcTrack, const Float_t *time,
		 const Float_t *x,const Float_t *y,const Float_t *z,
		 const Int_t *NoDaughters,const Int_t *IdParTrk,const Int_t *gePid) {
  Int_t Nvtx = NoVertices();
  for (Int_t l = 0; l < Nvtx; l++) {
    StKFVertex *V = Vertex(l);
    if (! V) continue;
    Int_t kv = V->IdTruth();
    if (kv > 0 && kv <= NoMuMcVertex) {
      if (time && x && y && z && NoDaughters && IdParTrk) {
	Int_t kvp = IdParTrk[kv-1];
	Int_t ge = 0;
	if (kvp > 0 && kvp <= NoMuMcTrack) ge = gePid[kvp-1];
	V->SetMc(time[kv-1],x[kv-1],y[kv-1],z[kv-1],NoDaughters[kv-1],ge);
      }
    }
  }
}
//________________________________________________________________________________
ostream&  operator<<(ostream& os,  const StKFVerticesCollection& vc) {
  Int_t Nvtx = vc.NoVertices();
  for (Int_t l = 0; l < Nvtx; l++) {
    const StKFVertex *V = vc.Vertex(l);
    if (! V) continue;
    os << Form("Vtx: %3i",l) << *V << endl;
  }
  return os;
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::DoTrack2VertexAssociation(const TObjArray &particles) {
  Double_t chi2Total = 0;
  Int_t NVtx    = NoVertices();
  Int_t Ntracks = particles.GetEntriesFast();
  TArrayI Idx(Ntracks);
  TArrayD Chi2s(Ntracks);
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
    //    if (! vtx->Vertex()) continue;
    //    vtx->PrintW();
    KFParticle *particle = 0;
    for (Int_t k = 0; k < Ntracks; k++) {
      Chi2s[k] = 1e10;
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (particle->GetID() > 100000) continue;
      Double_t chi2il = particle->GetDeviationFromVertex(vtx->Vertex());
      chi2il *= 2*chi2il;
      Chi2s[k] = chi2il;
    }
    vtx->Clear();    
    TMath::Sort(Ntracks,Chi2s.GetArray(),Idx.GetArray(),0);
    Double_t chi2Vx = 0;
    for (Int_t j = 0; j < Ntracks; j++) {
      Int_t k = Idx[j];
      particle = (KFParticle *) particles.UncheckedAt(k);
      if (! particle) continue;
      if (Chi2s[k] > StAnneling::Chi2Cut()) break;
      StKFTrack *track = new StKFTrack(k,particle,Chi2s[k]);
      chi2Vx += track->Chi2()/2 + TMath::Log(track->Weight() + StAnneling::Weight());
      vtx->AddTrack(track);
    }
    if (vtx->NoTracks() < 2) {
      delete vtx; Vertex(l) = 0;
      continue;
    }
    if (StKFVertex::Debug()) vtx->PrintW("DoTrack2VertexAssociation ");
    chi2Total += chi2Vx;
    //    vtx->PrintW();
  }
  fVertices.Compress();
  if (NoVertices()) UpdateWeights();
  return chi2Total;
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::UpdateStVertexTrackAssociation() {
  Double_t chi2Total = 0;
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
    chi2Total += vtx->UpdateVertex2TrackChi2();
  }
  return chi2Total;
}
//________________________________________________________________________________
void StKFVerticesCollection::CleanDuplicatedVertices() {
  Int_t NVtx = NoVertices();
  // Check that vertices are the same
  for (Int_t l = 1; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    Int_t NL = vtxl->NoTracks();
    TRVector vL(3,vtxl->Vertex().Parameters());
    TRSymMatrix CL(3,vtxl->Vertex().CovarianceMatrix());
    for (Int_t m = 0; m < l; m++) {
      StKFVertex *vtxm = Vertex(m);
      if (! vtxm) continue;
      // compliete overlap by an other vertex
      Int_t NM = vtxm->NoTracks();
      Int_t Nmatched = 0;
      for (Int_t i = 0; i < NL; i++) 
	for (Int_t j = 0; j < NM; j++) 
	  if (vtxl->Track(i)->OrigParticle() == vtxm->Track(j)->OrigParticle()) Nmatched++;
      if (Nmatched == TMath::Min(NL,NM)) {
	TRVector vM(3,vtxm->Vertex().Parameters());
	TRSymMatrix CM(3,vtxm->Vertex().CovarianceMatrix());
	vM -= vL;
	CM += CL;
	TRSymMatrix G(CM,TRArray::kInverted);
	Double_t chi2 = G.Product(vM,TRArray::kATxSxA);
	Double_t prob = TMath::Prob(chi2,3);
	if (prob > 0.10) {
	  if ((NL > NM) || ((NL == NM) && (vtxl->Vertex().GetChi2() < vtxm->Vertex().GetChi2()))) {
	    if (StKFVertex::Debug()) {
	      vtxm->Print(Form("Cleaned Vertex prob %7.2f M %3i keep L %3i L(%3i/%7.2f) M (%3i/%7.2f)\t",
			       prob,m,l,NL,vtxl->Vertex().GetChi2(),NM,vtxm->Vertex().GetChi2()));
	    }
	    delete vtxm; Vertex(m) = 0;
	    continue;
	  } else {
	    if (StKFVertex::Debug()) {
	      vtxl->Print(Form("Cleaned Vertex prob %7.2f L %3i keep M %3i M(%3i/%7.2f) L (%3i/%7.2f)",
			       prob,l,m,NM,vtxm->Vertex().GetChi2(),NL,vtxl->Vertex().GetChi2()));
	    }
	    delete vtxl; Vertex(l) = 0;
	    break;
	  }
	}
      }
    }
  }
  fVertices.Compress();
}
//________________________________________________________________________________
void StKFVerticesCollection::MergeDuplicatedVertices() {
  // Check that vertices are the same
  for (Int_t l = 1; l < NoVertices(); l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    if (! vtxl->NoTracks()) {delete vtxl; vtxl = Vertex(l) = 0; continue;}
    TRVector vL(3,vtxl->Vertex().Parameters());
    TRSymMatrix CL(3,vtxl->Vertex().CovarianceMatrix());
    for (Int_t m = 0; m < l; m++) {
      StKFVertex *vtxm = Vertex(m);
      if (! vtxm) continue;
      if (! vtxm->NoTracks()) {delete vtxm; vtxm = Vertex(m) = 0; continue;}
      TRVector vM(3,vtxm->Vertex().Parameters());
      vM -= vL;
      if (vM.Mag() > 5.) continue;
      TRSymMatrix CM(3,vtxm->Vertex().CovarianceMatrix());
      CM += CL;
      CM[0] += 0.0001; // 100 mkm tolerance
      CM[2] += 0.0001;
      CM[5] += 0.0001;
      TRSymMatrix G(CM,TRArray::kInverted);
      Double_t chi2 = G.Product(vM,TRArray::kATxSxA);
      Double_t prob = TMath::Prob(chi2,3);
      if (prob > 1e-4) {
	*vtxm += *vtxl;
	//	if (vtxl->NoTracks()) vtxl->Clear("keep");
	delete vtxl; vtxl = Vertex(l) = 0;
	vtxm->Fit();
	break;
      }
    }
  }
  fVertices.Compress();
}
//________________________________________________________________________________
void StKFVerticesCollection::UpdateWeights() {
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    // recalculate weights
    if (StKFVertex::Debug()) vtxl->PrintW("Weights to Update ");
    for (Int_t i = 0; i < vtxl->NoTracks(); i++) {
      Double_t Dominator = TMath::Exp(-StAnneling::Chi2Cut()/(2*StAnneling::Temperature())) + vtxl->Track(i)->Weight();
      for (Int_t m = 0; m < NVtx; m++) {
	if (l == m) continue;
	StKFVertex *vtxm = Vertex(m);
	if (! vtxm) continue;
	for (Int_t j = 0; j < vtxm->NoTracks(); j++) {
	  if (vtxl->Track(i)->OrigParticle() == vtxm->Track(j)->OrigParticle()) {
	    Dominator += vtxm->Track(j)->Weight();
	    break;
	  }
	}
      }
      vtxl->Track(i)->W() = vtxl->Track(i)->Weight()/Dominator;
      vtxl->Track(i)->Particle() = *(vtxl->Track(i)->OrigParticle());
      Double_t *CovXyz  =                 vtxl->Track(i)->Particle().CovarianceMatrix();
      for (Int_t j = 0; j < 36; j++) CovXyz[j] = CovXyz[j]/vtxl->Track(i)->W();
    }
    if (StKFVertex::Debug()) vtxl->PrintW("Updated Weights ");
  }
}
//________________________________________________________________________________
void StKFVerticesCollection::UniqueTracks2VertexAssociation(){
  // Make track associated with only vertex (by maximum weight to the vertex) 
  Int_t NVtx = NoVertices();
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    // recalculate weights
    for (Int_t i = vtxl->NoTracks()-1; i >= 0; i--) {
      if (! vtxl->Track(i)) continue;
      Double_t WMax = vtxl->Track(i)->Weight();
      Int_t    iMax = i;
      Int_t    lMax = l;
      Int_t    nPart = 1;
      KFParticle *particleMax = vtxl->Track(i)->OrigParticle();
      for (Int_t m = 0; m < NVtx; m++) {
	if (l == m) continue;
	StKFVertex *vtxm = Vertex(m);
	if (! vtxm) continue;
	for (Int_t j = 0; j < vtxm->NoTracks(); j++) {
	  if (particleMax == vtxm->Track(j)->OrigParticle()) {
	    nPart++;
	    if (vtxm->Track(j)->Weight() > WMax) {
	      WMax = vtxm->Track(j)->Weight();
	      iMax = j;
	      lMax = m;
	      particleMax = vtxm->Track(j)->OrigParticle();
	      break;
	    }
	  }
	}
      }
      if (WMax < 0.01) { // Particle weight is too small => remove the particle from all vertices
	for (Int_t m = 0; m < NVtx; m++) {
	  StKFVertex *vtxm = Vertex(m);
	  if (! vtxm) continue;
	  delete vtxm->Remove(particleMax);
	  vtxm->Compress();
	}
	if (! vtxl->NoTracks()) break;
	continue;
      }
      if (nPart > 1) {
	for (Int_t m = 0; m < NVtx; m++) {
	  StKFVertex *vtxm = Vertex(m);
	  if (! vtxm) continue;
	  if (m != lMax) {
	    if (particleMax->GetID()%100000) { // beam track is not in the game
	      delete vtxm->Remove(particleMax);
	      vtxm->Compress();
	      if (vtxm->NoTracks() == 0) {delete vtxm; Vertex(m) = 0;}
	    }
	  } else vtxm->Track(iMax)->W() = vtxm->Track(iMax)->Weight();
	}
      }
    }
  }
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    if (vtxl->NoTracks() == 0) {delete vtxl; Vertex(l) = 0;}
  }
  fVertices.Compress();
  NVtx = NoVertices();
  // Set particle ID
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtxl = Vertex(l);
    if (! vtxl) continue;
    Int_t N = vtxl->NoTracks();
    for (Int_t i = 0; i < N; i++) {
      KFParticle *particle = vtxl->Track(i)->OrigParticle();;
      Int_t ID = particle->GetID()%100000 + 100000*vtxl->ID();;
      particle->SetID(ID);
    }
  }
}
//________________________________________________________________________________
Double_t StKFVerticesCollection::Fit(Int_t marker, TCanvas *c1, TH1 *Vtx) {
  // Primary Vertex fit
  Double_t chi2Total = 1e10;
  fVertices.Compress();
  Int_t NVtx = NoVertices();
  if (! NVtx) return chi2Total; 
  for (Int_t l = 0; l < NVtx; l++) {
    StKFVertex *vtx = Vertex(l);
    if (! vtx) continue;
    vtx->Vertex().SetBeamConstraintOff();
    vtx->Fit();
    if (vtx->Vertex().GetNDF() < 1 || vtx->NoTracks() < 2) {
      delete vtx;
      Vertex(l) = 0;
      continue;
    } else {
      chi2Total += fgVxPenaltyFactor;
    }
  }
  fVertices.Compress();
  CleanDuplicatedVertices();
  fVertices.Compress();
  chi2Total = UpdateStVertexTrackAssociation();
  UpdateWeights();
  //
  if (StKFVertex::Debug()) {
    cout << "chi2Total = " << chi2Total 
	 << " at Temperature " << StAnneling::Temperature() 
	 << " and Log(Temperature) " << TMath::Log(StAnneling::Temperature()) 
	 << " no. vertices " << NoVertices()
	 << endl;
  }
  if (Vtx) {
    Double_t ymax = Vtx->GetMaximum();
    for (Int_t i = 0; i < NVtx; i++) {
      StKFVertex *vtx = Vertex(i);
      if (! vtx) continue;
      Double_t X = vtx->Vertex().GetParameter(2);
      Double_t Y = vtx->NoTracks();
      if (Y > ymax) ymax = Y;
      TPolyMarker * pm = new TPolyMarker(1, &X, &Y);
      Vtx->GetListOfFunctions()->Add(pm);
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
      Vtx->SetMaximum(ymax);
      Vtx->Draw("same");
      c1->Update();
    }
  }
  return chi2Total;
}
// $Log: StKFVerticesCollection.cxx,v $
// Revision 2.5  2018/04/10 11:32:10  smirnovd
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
// Revision 1.5  2012/03/29 23:35:47  fisyak
// Fix problem with multiple beam tracks
//
// Revision 1.4  2012/03/26 23:42:35  fisyak
// Add beam constrain
//
// Revision 1.3  2012/02/07 19:38:26  fisyak
// Repackage
//
