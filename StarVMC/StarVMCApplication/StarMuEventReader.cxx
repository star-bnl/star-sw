#include "StarMuEventReader.h"
#include "StarGenerator/EVENT/StarGenEvent.h"
#include "StarGenerator/EVENT/StarGenParticle.h"
#include "TUnixTime.h"
#include "StMaker.h"
#include "StEvtHddr.h"
#include "TDatabasePDG.h"
//________________________________________________________________________________
void StarMuEventReader::SetMuDstFile(const Char_t *muDstFile) {
  TString MuDstF(muDstFile);
  fMuDstIter = new TTreeIter();
  fMuDstIter->AddFile(MuDstF);
  ReadEvent(1);
}
//________________________________________________________________________________
Int_t StarMuEventReader::Generate() {
  return ReadEvent();
}
//________________________________________________________________________________
Int_t StarMuEventReader::ReadEvent(Int_t N) 
{
  static const Int_t*&      RunId                                    = (*fMuDstIter)("MuEvent.mEventInfo.mRunId");
  static const Int_t*&      Id                                       = (*fMuDstIter)("MuEvent.mEventInfo.mId");
  static const Int_t*&      MuEvent_mEventInfo_mTime                 = (*fMuDstIter)("MuEvent.mEventInfo.mTime"); 
  //  static const Int_t*&      MuEvent_mEventSummary_mNumberOfTracks    = (*fMuDstIter)("MuEvent.mEventSummary.mNumberOfTracks");
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
  static const Int_t&       NoGlobalTracks                           = (*fMuDstIter)("GlobalTracks");
  static const Short_t*&    GlobalTracks_mFlag                       = (*fMuDstIter)("GlobalTracks.mFlag");
#define __USE_GLOBAL__
#ifdef __USE_GLOBAL__
  static const Float_t*&    GlobalTracks_mP_mX1                      = (*fMuDstIter)("GlobalTracks.mP.mX1");
  static const Float_t*&    GlobalTracks_mP_mX2                      = (*fMuDstIter)("GlobalTracks.mP.mX2");
  static const Float_t*&    GlobalTracks_mP_mX3                      = (*fMuDstIter)("GlobalTracks.mP.mX3");
  static const Short_t*&    GlobalTracks_mHelix_mQ                   = (*fMuDstIter)("GlobalTracks.mHelix.mQ");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX1             = (*fMuDstIter)("GlobalTracks.mFirstPoint.mX1");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX2             = (*fMuDstIter)("GlobalTracks.mFirstPoint.mX2");
  static const Float_t*&    GlobalTracks_mFirstPoint_mX3             = (*fMuDstIter)("GlobalTracks.mFirstPoint.mX3");
  static const Int_t*&      GlobalTracks_mNSigmaElectron             = (*fMuDstIter)("GlobalTracks.mNSigmaElectron");
  static const Int_t*&      GlobalTracks_mNSigmaPion                 = (*fMuDstIter)("GlobalTracks.mNSigmaPion");
  static const Int_t*&      GlobalTracks_mNSigmaKaon                 = (*fMuDstIter)("GlobalTracks.mNSigmaKaon");
  static const Int_t*&      GlobalTracks_mNSigmaProton               = (*fMuDstIter)("GlobalTracks.mNSigmaProton");
#endif
  static const Double_t __SIGMA_SCALE__ = 1000.;
 NEXT:
  if (! fMuDstIter->Next()) {fStatus =  kStEOF; return fStatus;}
  Int_t id, it;
  TUnixTime ut(MuEvent_mEventInfo_mTime[0]); ut.GetGTime(id,it);
  StEvtHddr *fEvtHddr = (StEvtHddr*) StMaker::GetChain()->GetDataSet("EvtHddr");
  if (fEvtHddr) {                            // Standalone run
    fEvtHddr->SetDateTime(id,it);
    fEvtHddr->SetRunNumber(RunId[0]);
    fEvtHddr->SetEventNumber(Id[0]);
    if (Debug()) fEvtHddr->Print();
  }
  if (N) {
    fMuDstIter->Reset();
    return kStOk;
  }
  Int_t nvtx = 0;
  Int_t ntrack = 0;
  Int_t types[5][2] = {
    { -11,  11},
    { 321, -321},
    {2212,-2212},
    { 211, -211},
    { -22,  22}
  };
  if (Debug()) {
    cout << "NoPrimaryVertices = " << NoPrimaryVertices 
	 << "\tNoPrimaryTracks = " << NoPrimaryTracks
	 << "\tNoGlobalTracks  = " << NoGlobalTracks << endl;
  }
  fStarStack->Reset();
  // Option: to be tracked
  Int_t toBeDone = 1; 
  Double_t polx = 0.; 
  Double_t poly = 0.; 
  Double_t polz = 0.; 
  Double_t px = 0, py = 0, pz = 0;
  Double_t vx = 0, vy = 0, vz = 0;
  Double_t tof = 0.;
  Double_t e, mass;
  Int_t t = 3;
  Int_t pdg = 0;
  Double_t nSigmaMin = 1e9;
  for (Int_t l = 0; l < NoPrimaryVertices; l++) {
    if (l) continue; // only 1st primary vertex
    vx = PrimaryVertices_mPosition_mX1[l];
    vy = PrimaryVertices_mPosition_mX2[l];
    vz = PrimaryVertices_mPosition_mX3[l];
    nvtx++;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (l != PrimaryTracks_mVertexIndex[k]) continue;
      Int_t kg = PrimaryTracks_mIndex2Global[k];
      if (GlobalTracks_mFlag[kg] < 100) continue;
      px = PrimaryTracks_mP_mX1[k];
      py = PrimaryTracks_mP_mX2[k];
      pz = PrimaryTracks_mP_mX3[k];
      Double_t nSigma[4] = {
	PrimaryTracks_mNSigmaElectron[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaKaon[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaProton[k]/__SIGMA_SCALE__,
	PrimaryTracks_mNSigmaPion[k]/__SIGMA_SCALE__};
      Int_t s = 0;
      if (PrimaryTracks_mHelix_mQ[k] < 0) s = 1;
      t = 3;
      nSigmaMin = 1e9;
      for (Int_t i = 0; i < 4; i++) {
	if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	  nSigmaMin = TMath::Abs(nSigma[i]); 
	  t = i;
	}
      }
      if (nSigmaMin > 4) {t = 3;} // for pion 
      pdg = types[s][t];
      mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
      e  = TMath::Sqrt(mass*mass + px*px + py*py + pz*pz);
      ntrack++;
      // Add particle to stack 
      fStarStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
			    kPPrimary, ntrack, 1., 2);
    }
  }
#ifdef __USE_GLOBAL__
  for (Int_t kg = 0; kg < NoGlobalTracks; kg++) {
    if (GlobalTracks_mFlag[kg] < 100) continue;
    nSigmaMin = 1e9;
    Double_t nSigma[4] = {
      GlobalTracks_mNSigmaElectron[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaKaon[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaProton[kg]/__SIGMA_SCALE__,
      GlobalTracks_mNSigmaPion[kg]/__SIGMA_SCALE__};
    Int_t s = 0;
    if (GlobalTracks_mHelix_mQ[kg] < 0) s = 1;
    for (Int_t k = 0; k < NoPrimaryTracks; k++) {
      if (kg == PrimaryTracks_mIndex2Global[k]) {
	goto NEXTGL;
      }
    }
    vx = GlobalTracks_mFirstPoint_mX1[kg];
    vy = GlobalTracks_mFirstPoint_mX2[kg];
    vz = GlobalTracks_mFirstPoint_mX3[kg];
    px = GlobalTracks_mP_mX1[kg];
    py = GlobalTracks_mP_mX2[kg];
    pz = GlobalTracks_mP_mX3[kg];
    t = 4;
    for (Int_t i = 0; i < 4; i++) {
      if (TMath::Abs(nSigma[i]) <  nSigmaMin) {
	nSigmaMin = TMath::Abs(nSigma[i]); 
	t = i;
      }
    }
    if (nSigmaMin > 3) {t = 4;} // force muon
    pdg = types[s][t];
    mass = TDatabasePDG::Instance()->GetParticle(pdg)->Mass();
    e  = TMath::Sqrt(mass*mass + px*px + py*py + pz*pz);
    ntrack++;
    // Add particle to stack 
    fStarStack->PushTrack(toBeDone, -1, pdg, px, py, pz, e, vx, vy, vz, tof, polx, poly, polz, 
			  kPPrimary, ntrack, 1., 2);
  NEXTGL:
    continue;
  }
#endif
  if ( Debug() ) fStarStack->Print();
  if (! ntrack) goto NEXT;
  return kStOK;
};
//________________________________________________________________________________
Int_t StarMuEventReader::Skip(Int_t Nskip) {
  for (Int_t i = 0; i < Nskip; i++) {
    if (ReadEvent()) return kStEOF; 
  }
  return kStOK;
}
