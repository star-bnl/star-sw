void makeStatusPlots(TString dir="/star/data07/EMC/staszak/2006pp/longpre/day079/", 
		     //TString dir="/star/institutions/ucla/staszak/bemc2006pp_pass2/longpost3/day154/special/", 
                     TString filter=".cal.total.hist.root", 
		     const TString plotDir="/star/data07/EMC/staszak/2006pp/longpre/day079/out/") {
		     //const TString plotDir="/star/institutions/ucla/staszak/bemc2006pp_pass2/longpost3/day154/special/out/") {
  //
  // load shared libs
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
  assert(gSystem->Load("StEmcPoolCSMStatusUtils")==0);
  cout<<"All libs loaded"<<endl;
  

  // all libs loaded  
  
  CSMStatusUtils myUtils;
  myUtils.initializeHistFileFromDir(dir,filter);
  myUtils.setDetectorFlavor("bemc");
  myUtils.makeStatusPlots(plotDir);  
  //myUtils.setDetectorFlavor("eemc");
  //myUtils.makeStatusPlots(plotDir);  
}
