/**
 Load STAR shared libraries

 If personal == true load project-specific libraries
 */
void load(Bool_t personal=true) {
  // STAR libraries for chain, MuDST, logger etc
  gROOT->Macro("loadMuDst.C");
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gROOT->Macro("LoadLogger.C");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StRandomSelector");
  gSystem->Load("StTriggerFilterMaker");
  gSystem->Load("StEventMaker");
  if (personal) {  // Load our shared libraries here
    gSystem->Load("libMinuit.so");
    gSystem->Load("StFmsUtil");
    gSystem->Load("StFmsDbMaker");
    gSystem->Load("StFmsHitMaker");
    gSystem->Load("StFmsPointMaker");
    //    gSystem->Load("StFmsQAHistoMaker");
    gSystem->Load("StFmsFpsMaker");
  }  // if
}
