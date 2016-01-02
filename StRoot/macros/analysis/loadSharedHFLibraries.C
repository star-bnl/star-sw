#include "TSystem.h"

#ifndef __CINT__
#include "StMuDSTMaker/COMMON/macros/loadSharedLibraries.C"
#endif

extern TSystem* gSystem;

void loadSharedHFLibraries() {

  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();

  gSystem->Load("StBTofUtil");
  gSystem->Load("StPicoDstMaker");
  gSystem->Load("StPicoCutsBase");
  gSystem->Load("StPicoPrescales");
  gSystem->Load("StPicoHFMaker");
  gSystem->Load("StPicoHFMyAnaMaker");
    gSystem->Load("StRefMultCorr");
  gSystem->Load("StPicoMixedEventMaker");
  cout << " loading of shared HF libraries are done" << endl;

  // -->>> ADD your own library/class HERE 

 }
