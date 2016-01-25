// $Id: $
#include "StMuD0AnalysisMaker.h"
#include "TChain.h"
#include "TFile.h"
#include "TArrayI.h"
#include "TLorentzVector.h"
#include "TVector.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuPrimaryVertex.h"
#include "StMuDSTMaker/COMMON/StMuMcVertex.h"
#include "StBTofHeader.h"
#include "StDcaGeometry.h"
#include "KFParticle/KFVertex.h"
#include "KFParticle/KFPTrack.h"
ClassImp(StMuD0AnalysisMaker);
//_____________________________________________________________________________
Int_t StMuD0AnalysisMaker::Init(){
  TFile  *f = GetTFile();
  assert(f);
  f->cd();
  Int_t split  = 99;       // by default, split Event in sub branches
  Int_t comp   = 1;       // by default file is compressed
  Int_t branchStyle = 1; //new style by default
  if (split < 0) {branchStyle = 0; split = -1-split;}
  Int_t bufsize;
  //Authorize Trees up to 2 Terabytes (if the system can do it)
  TTree::SetMaxTreeSize(1000*Long64_t(2000000000));
  // Create a ROOT Tree and one superbranch
  fTree = new TTree("T","TTree for Kpi Pairs");
  fTree->SetAutoSave(1000000000);  // autosave when 1 Gbyte written
  bufsize = 64000;
  if (split)  bufsize /= 4;
  fEvent = new D0Event();
  TTree::SetBranchStyle(branchStyle);
  TBranch *branch = fTree->Branch("EventT", &fEvent, bufsize,split);
  branch->SetAutoDelete(kFALSE);
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StMuD0AnalysisMaker::Make(){
  StMuDstMaker *muDstMaker = StMuDstMaker::instance();
  if (! muDstMaker) return kStFatal;
  StMuDst *mu = muDstMaker->muDst();
  if (! mu) return kStErr;
  StMuEvent* muEvent = mu->event(); // get a pointer to the class holding event-wise information
  if (Debug()) {
    mu->Print();
    mu->printVertices();
    mu->printPrimaryTracks();
    mu->printGlobalTracks();
    mu->printKFVertices();
    mu->printKFTracks();
    mu->printMcVertices();
    mu->printMcTracks();
  }
  if (mu->numberOfPrimaryVertices() == 0) return kStOK;;
  fEvent->Set(muEvent->eventId(),muEvent->runId());
  KFParticle::SetField(muEvent->magneticField());
  Double_t VpdZ = -9999;
  StBTofHeader* BTofHeader = mu->btofHeader();
  if ( BTofHeader) {
    UInt_t NoWestHits = BTofHeader->numberOfVpdHits(west);
    UInt_t NoEastHits = BTofHeader->numberOfVpdHits(east);
    if ( NoWestHits > 0 &&  NoEastHits > 0) {
      VpdZ = BTofHeader->vpdVz();
    }
  }
  if (TMath::Abs(VpdZ) > 10) return kStOK;
  if (! mu->primaryVertex(0)) return kStOK;
  const Int_t IndxHighestRankVx = 0;
  Int_t IdPV = mu->primaryVertex(IndxHighestRankVx)->id(); //higest rank vertex
  if (TMath::Abs(mu->primaryVertex(IndxHighestRankVx)->position().z()) > 6.0) return kStOK;
  UInt_t NKFV = mu->numberOfKFVertices();
  KFVertex *KFV = 0;
  Float_t dZ = 1e8;
  for (UInt_t i = 0; i < NKFV; i++) {
    KFVertex *kfv = mu->KFvertex(i);
    if (IdPV != kfv->Id()) continue;
    Double_t dz = kfv->GetZ() - VpdZ;
    if (TMath::Abs(dz) < dZ) {
      dZ = TMath::Abs(dz);
      KFV = kfv;
    }
  }
  if (dZ > 1.0) return kStOK;
  if (! KFV)    return kStOK;
  UInt_t NoGlobalTracks = mu->numberOfGlobalTracks();
  // list of good global tracks
  TArrayI Gl2Pr(NoGlobalTracks); Int_t *gl2Pr = Gl2Pr.GetArray();
  for (UInt_t kg = 0; kg < NoGlobalTracks; kg++) { 
    StMuTrack *gTrack = mu->globalTracks(kg);
    if (gTrack->bad()) continue;
    if (gTrack->nHitsFit() < 15) continue;
    if (gTrack->pt() < 1.2) continue;
    if (TMath::Abs(gTrack->nSigmaPion()) > 3 &&
	TMath::Abs(gTrack->nSigmaKaon()) > 2.5) continue;
    const StMuProbPidTraits &pid = gTrack->probPidTraits();
    if (pid.dEdxTrackLength() < 40) continue;
    StTrackTopologyMap topologyMap = gTrack->topologyMap();
    UInt_t noPxlHits = topologyMap.numberOfHits(kPxlId); // 0-3
    UInt_t noIstHits = topologyMap.numberOfHits(kIstId); // 0-2
    UInt_t noSsdHits = topologyMap.numberOfHits(kSsdId); // 0-2
    UInt_t noHftHits = noPxlHits + noIstHits + noSsdHits;
    if (noPxlHits < 2 || noIstHits < 1) continue;
    gl2Pr[kg] = kg + 1;
  }
  // Remove primary tracks
  UInt_t NoPrimaryTracks = mu->numberOfPrimaryTracks();
  for (UInt_t k = 0; k < NoPrimaryTracks; k++) {
    StMuTrack *pTrack = mu->primaryTracks(k);
    if (  pTrack->vertexIndex() != IndxHighestRankVx) continue;
    Int_t kg = pTrack->index2Global();
    if (kg < 0) continue;
    gl2Pr[kg] = 0;
  }
  // Build K pi pairs
 
  Double_t xyzp[6], CovXyzp[21];
  for (UInt_t ig = 0; ig < NoGlobalTracks; ig++) { // pion 
    if (! gl2Pr[ig]) continue;
    StMuTrack *gTrackI = mu->globalTracks(ig);
    if (TMath::Abs(gTrackI->nSigmaPion()) > 3) continue;
    Int_t kgcI = gTrackI->index2Cov();
    const StDcaGeometry *dcaGI = mu->covGlobTracks(kgcI);
    if (! dcaGI) continue;
    dcaGI->GetXYZ(xyzp, CovXyzp);
    KFPTrack trackI;
    trackI.SetParameters(xyzp);
    trackI.SetCovarianceMatrix(CovXyzp);
    trackI.SetNDF(1);
    trackI.SetID(gTrackI->id());
    trackI.SetCharge(dcaGI->charge());
    Int_t pdgI = dcaGI->charge() > 0 ? 211 : -211; // assume all tracks are pions.
    KFParticle pion(trackI, pdgI);
    Float_t distPion = pion.GetDistanceFromVertex(*KFV);
    for (UInt_t jg = 0; jg < NoGlobalTracks; jg++) { // Kaon
      if (ig == jg) continue;
      if (! gl2Pr[jg]) continue;
      StMuTrack *gTrackJ = mu->globalTracks(jg);
      if (TMath::Abs(gTrackJ->nSigmaKaon()) > 2.5) continue;
      Int_t kgcJ = gTrackJ->index2Cov();
      const StDcaGeometry *dcaGJ = mu->covGlobTracks(kgcJ);
      if (! dcaGJ) continue;
      dcaGJ->GetXYZ(xyzp, CovXyzp);
      KFPTrack trackJ;
      trackJ.SetParameters(xyzp);
      trackJ.SetCovarianceMatrix(CovXyzp);
      trackJ.SetNDF(1);
      trackJ.SetID(gTrackJ->id());
      trackJ.SetCharge(dcaGJ->charge());
      Int_t pdgJ = dcaGJ->charge() > 0 ? 321 : -321; // assume all tracks are Kaons.
      KFParticle Kaon(trackJ, pdgJ);
      Float_t distKaon = Kaon.GetDistanceFromVertex(*KFV);
      // Make a pair
      KFParticle pair(pion,Kaon);
      if (pair.NDF() < 0 || pair.GetChi2() <= 0 || pair.GetChi2() > 5) continue;
      pair.SetProductionVertex(*KFV);
      if (pair.GetChi2() <= 0 || pair.GetChi2() > 5) continue;
      Float_t S, dS;
      Float_t T, dT;
      Float_t M, dM;
      if (pair.GetDecayLength(S,dS)) continue;
      if (pair.GetDecayLength(T,dT)) continue;
      if (pair.GetMass(M,dM)) continue;
      if (TMath::Abs(S/dS) < 5) continue;
      if (TMath::Abs(S) > 2) continue;
      TLorentzVector lKpi;
      lKpi.SetPxPyPzE(pair.Px(),pair.Py(),pair.Pz(),pair.E());
      TVector3 bF = lKpi.BoostVector();
      TVector3 b(-bF.X(),-bF.Y(),-bF.Z());
      TLorentzVector Kl(Kaon.Px(),Kaon.Py(),Kaon.Pz(),Kaon.E());
      Kl.Boost(b);
      TVector3 dother(Kl.Vect());
      TVector3 mother(lKpi.Vect());
      Float_t CosTheta = dother.Dot(mother)/(dother.Mag()*mother.Mag()); 
      KpiPair *p = fEvent->AddPair();
      
      p->pair = pair;
      p->M = M;
      p->dM = dM;
      p->S = S;
      p->dS = dS;
      p->T = T;
      p->dT = dT;
      p->distPion = distPion;
      p->distKaon = distKaon;
      p->CosTheta = CosTheta;
      p->chi2Vx = pair.GetChi2();
    }
  }
  if (fEvent->GetNPairs()) {
    fTree->Fill();
  }
  return kStOK;
}
// $Log: $
