void plotTiming(TString directory = "crate1/") {

  // load shared libs
  Bool_t useStarLibs = kTRUE;
  if (useStarLibs) {
    gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    loadSharedLibraries();
    gSystem->Load("StMcEvent");
    gSystem->Load("StMcEventMaker");
    gSystem->Load("StAssociationMaker");
    // load EMC libraries
    gSystem->Load("StDaqLib");
    gSystem->Load("StEmcRawMaker");
    gSystem->Load("StEmcADCtoEMaker");
    gSystem->Load("StPreEclMaker");
    gSystem->Load("StEpcMaker");
  }
  assert(gSystem->Load("StEEmcPooltimingCalibration")==0);
  cout<<"All libs loaded"<<endl;
  
  eemcTimingScanPlot tsp;
  tsp.setAxisRange(0.,110.); 
  tsp.scan(directory);
}
