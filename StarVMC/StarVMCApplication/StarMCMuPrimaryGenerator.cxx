// $Id: StarMCMuPrimaryGenerator.cxx,v 1.1.1.1 2008/12/10 20:45:52 fisyak Exp $
#include "StarMCMuPrimaryGenerator.h"
#include "tables/St_particle_Table.h"
#include "TDatabasePDG.h"
#include "TUnixTime.h"
#include "Stypes.h"
#include "StMaker.h"
#include "StEvtHddr.h"
#include "TVirtualMC.h"
#include "TMCProcess.h"
ClassImp(StarMCMuPrimaryGenerator);
//_____________________________________________________________________________
void StarMCMuPrimaryGenerator::Skip(Int_t Nskip)  {
  for (Int_t i = 0; i < Nskip; i++) {
    if (! fMuDstIter->Next()) break;
  }
}
//_____________________________________________________________________________
void StarMCMuPrimaryGenerator::GeneratePrimaries() {// generate primaries from MuOOK NTuple
  
  static const Int_t*&      MuEvent_mEventInfo_mRunId                = (*fMuDstIter)("MuEvent.mEventInfo.mRunId");
  static const Int_t*&      MuEvent_mEventInfo_mId                   = (*fMuDstIter)("MuEvent.mEventInfo.mId");
  static const Int_t*&      MuEvent_mEventInfo_mTime                 = (*fMuDstIter)("MuEvent.mEventInfo.mTime"); 
  static const Int_t*&      MuEvent_mEventSummary_mNumberOfTracks    = (*fMuDstIter)("MuEvent.mEventSummary.mNumberOfTracks");
  static const Int_t&       NoPrimaryVertices                        = (*fMuDstIter)("PrimaryVertices");
  static const Float_t*&    PrimaryVertices_mPosition_mX1            = (*fMuDstIter)("PrimaryVertices.mPosition.mX1");
  static const Float_t*&    PrimaryVertices_mPosition_mX2            = (*fMuDstIter)("PrimaryVertices.mPosition.mX2");
  static const Float_t*&    PrimaryVertices_mPosition_mX3            = (*fMuDstIter)("PrimaryVertices.mPosition.mX3");
  static const Int_t&       NoPrimaryTracks                          = (*fMuDstIter)("PrimaryTracks");
  static const Int_t*&      PrimaryTracks_mIndex2Global              = (*fMuDstIter)("PrimaryTracks.mIndex2Global");
  static const Int_t*&      PrimaryTracks_mVertexIndex               = (*fMuDstIter)("PrimaryTracks.mVertexIndex");
  static const Float_t*&    PrimaryTracks_mP_mX1                     = (*fMuDstIter)("PrimaryTracks.mP.mX1");
  static const Float_t*&    PrimaryTracks_mP_mX2                     = (*fMuDstIter)("PrimaryTracks.mP.mX2");
  static const Float_t*&    PrimaryTracks_mP_mX3                     = (*fMuDstIter)("PrimaryTracks.mP.mX3");
  static const Short_t*&    PrimaryTracks_mHelix_mQ                  = (*fMuDstIter)("PrimaryTracks.mHelix.mQ");
  static const Int_t*&      PrimaryTracks_mNSigmaElectron            = (*fMuDstIter)("PrimaryTracks.mNSigmaElectron");
  static const Int_t*&      PrimaryTracks_mNSigmaPion                = (*fMuDstIter)("PrimaryTracks.mNSigmaPion");
  static const Int_t*&      PrimaryTracks_mNSigmaKaon                = (*fMuDstIter)("PrimaryTracks.mNSigmaKaon");
  static const Int_t*&      PrimaryTracks_mNSigmaProton              = (*fMuDstIter)("PrimaryTracks.mNSigmaProton");
  static const Short_t*&    GlobalTracks_mFlag                       = (*fMuDstIter)("GlobalTracks.mFlag");
  static const Double_t __SIGMA_SCALE__ = 1000.;
  if (! fMuDstIter->Next()) {fStatus =  kStEOF; return;}
  Int_t id, it;
  TUnixTime ut(MuEvent_mEventInfo_mTime[0]); ut.GetGTime(id,it);
  StEvtHddr *fEvtHddr = (StEvtHddr*) StMaker::GetChain()->GetDataSet("EvtHddr");
  if (fEvtHddr) {                            // Standalone run
    fEvtHddr->SetDateTime(id,it);
    fEvtHddr->SetRunNumber(MuEvent_mEventInfo_mRunId[0]);
    fEvtHddr->SetEventNumber(MuEvent_mEventInfo_mId[0]);
  }
  Float_t v[4];
  Float_t plab[4];
  Float_t pol[3];
  Int_t nvtx = 0;
  Int_t ntr = 0;
  Int_t ipart;
  static Int_t pId[4][2] = {
    { 2, 3}, // e+, e-
    {11,12}, // K+, K-
    {14,15}, // p, pbar
    { 8, 9}  // pi+, pi-
  };
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {
    v[0] = PrimaryVertices_mPosition_mX1[l];
    v[1] = PrimaryVertices_mPosition_mX2[l];
    v[2] = PrimaryVertices_mPosition_mX3[l];
    v[3] = 0;
    nvtx++;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (l != PrimaryTracks_mVertexIndex[k]) continue;
      Int_t kg = PrimaryTracks_mIndex2Global[k];
      if (GlobalTracks_mFlag[kg] < 100) continue;
      plab[0] = PrimaryTracks_mP_mX1[k];
      plab[1] = PrimaryTracks_mP_mX2[k];
      plab[2] = PrimaryTracks_mP_mX3[k];
#ifndef __MUONS__
      Double_t nSigma[4] = {
	PrimaryTracks_mNSigmaElectron[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaKaon[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaProton[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaPion[k]/__SIGMA_SCALE__};
      Int_t s = 0;
      if (PrimaryTracks_mHelix_mQ[k] < 0) s = 1;
      ipart = pId[3][s];
      Double_t nSigmaMin = 1e9;
      for (Int_t i = 0; i < 4; i++) {
	if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	  nSigmaMin = TMath::Abs(nSigma[i]); 
	  ipart = pId[i][s];
	}
      }
      if (nSigmaMin < 2) ipart = pId[3][s];
#else
      ipart = 5; // muon+
      if (PrimaryTracks_mHelix_mQ[k] < 0) ipart = 6; // muon-
#endif
      Int_t pdg = gMC->PDGFromId(ipart);
      Double_t mass = gMC->ParticleMass(pdg);
      plab[3] = TMath::Sqrt(mass*mass + plab[0]*plab[0] + plab[1]*plab[1] + plab[2]*plab[2]);
      Int_t toBeDone = 1;
      Int_t parent = 0;
      Float_t tof = 0;
      ntr++;
      fStarStack->PushTrack(toBeDone, parent, pdg, 
			    plab,// px, py, pz, e, 
			    v,   //vx, vy, vz, tof, (mm->cm) 
			    pol, tof,
			    kPPrimary, ntr, 1., 2); // mech, &ntr, weight, status
    }
  }
  Int_t NPrimary = fStarStack->GetNtrack();
  fStarStack->SetNprimaries(NPrimary);
}
