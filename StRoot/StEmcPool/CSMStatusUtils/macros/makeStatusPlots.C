void makeStatusPlots(TString dir="/star/u/relyea/star/bemc/abemc/", 
                     TString filter=".cal.total.hist.root", 
		     const TString plotDir="/star/u/relyea/star/bemc/abemc/") {
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
  myUtils.setDetectorFlavor("eemc");
  myUtils.makeStatusPlots(plotDir);  
//  myUtils.setDetectorFlavor("eemc");
//  myUtils.makeStatusPlots(plotDir);  
}
