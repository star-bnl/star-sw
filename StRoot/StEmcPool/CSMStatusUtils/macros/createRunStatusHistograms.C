void createRunStatusHistograms(const Char_t* directory =
"/star/u/relyea/star/bemc/abemc/",  const Char_t* filter = "cal.minirun.root") {

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
  assert(gSystem->Load("StEmcPoolCSMStatusUtils")==0);
  cout<<"All libs loaded"<<endl;
  
  CSMBuildRunMap builder;
  builder.buildRunMap(directory,filter);
//  builder.buildRunMap(directory);
}
