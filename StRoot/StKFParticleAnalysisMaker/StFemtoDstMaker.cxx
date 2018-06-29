// $Id: StFemtoDstMaker.cxx,v 1.18 2007/10/27 17:42:59 fine Exp $
// $Log: StFemtoDstMaker.cxx,v $
#include "StFemtoDstMaker.h"
#include "StPicoDstMaker/StPicoDstMaker.h"
#include "StPicoEvent/StPicoDst.h"
#include "StPicoEvent/StPicoArrays.h"
#include "StChain/StChainOpt.h"
#include "StKFParticleInterface.h"
#include "TSystem.h"
#include "TClonesArray.h"
#include "TArrayC.h"
ClassImp(StFemtoDstMaker);
//_____________________________________________________________________________
StFemtoDstMaker::~StFemtoDstMaker() {delete fStKFParticleInterface;}
//_____________________________________________________________________________
Int_t StFemtoDstMaker::Init(){
  TTree *oldtree = StPicoDstMaker::instance()->tree();
  assert(oldtree);
  TString fileOut = GetChainOpt()->GetFileOut();
  assert(fileOut != "");
  fileOut = gSystem->BaseName(fileOut);
  Int_t l = fileOut.Index(".");
  if (l < 0) l = fileOut.Length();
  fileOut = TString(fileOut.Data(),l);
  fileOut.ReplaceAll("*","");
  fileOut += ".femtoDst.root";
  fOutFile = TFile::Open(fileOut,"recreate");
  fFemtoTree = oldtree->CloneTree(0);
  fFemtoTree->CopyAddresses(oldtree);
  fFemtoTree->AutoSave();
  fStKFParticleInterface = new StKFParticleInterface;
  fStKFParticleInterface->SetTriggerMode(); 
  fStKFParticleInterface->SetSoftKaonPIDMode(); 
  fStKFParticleInterface->SetSoftTofPidMode(); 
  fStKFParticleInterface->SetChiPrimaryCut(10); //10); // 8 // 12
  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StFemtoDstMaker::Make(){
  fFemtoDst = StPicoDst::instance();
  if (! fFemtoDst) return kStOK; 
  vector<int> triggeredTracks; 
  bool isGoodEvent = fStKFParticleInterface->ProcessEvent(fFemtoDst, triggeredTracks); 
  bool openCharmTrigger = false; 
  if(isGoodEvent) openCharmTrigger = fStKFParticleInterface->OpenCharmTrigger();
  if (! openCharmTrigger) {
    for (UInt_t i = StPicoArrays::Track; i < StPicoArrays::NAllPicoArrays; i++) {
#ifdef __TFG__VERSION__
      StPicoDstMaker::instance()->picoArrays()[i]->Clear();
#else /* ! __TFG__VERSION__ */
      StPicoDst::instance()->picoArray(i)->Clear();
#endif /* __TFG__VERSION__ */
    }
  } else {
    TClonesArray *tracks = StPicoDst::instance()->picoArray(StPicoArrays::Track);
    Int_t N = tracks->GetEntriesFast();
    TArrayC flags(N);
    for (UInt_t i = 0; i < triggeredTracks.size(); i++) flags[triggeredTracks[i]] = kTRUE;
    for (Int_t i = 0; i < N; i++) {
      if (! flags[i]) tracks->RemoveAt(i);
    }
    tracks->Compress();
  }
  fFemtoTree->Fill();
  return kStOK;
}
//________________________________________________________________________________
Int_t StFemtoDstMaker::Finish() {
  if (fOutFile) {
    fOutFile->Write();
    fOutFile->Flush();
    SafeDelete(fOutFile);
  }
  return StMaker::Finish();
}
