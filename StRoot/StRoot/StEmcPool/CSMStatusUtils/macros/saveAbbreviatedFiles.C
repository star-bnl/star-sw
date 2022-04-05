void saveAbbreviatedFiles(//const Char_t *dir="/star/institutions/ucla/staszak/bemc2006pp_pass2/trans3/test_root/") {
			  const Char_t *dir="/star/data07/EMC/staszak/2006pp/trans/test/") {
  //
  // load shared libs
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  gSystem->Load("St_Tables.so");
  gSystem->Load("StDbLib.so");
  gSystem->Load("StDbBroker.so");
  gSystem->Load("St_db_Maker.so");
  gSystem->Load("St_base");
  gSystem->Load("StUtilities");  
  gSystem->Load("StarClassLibrary");
  gSystem->Load("StChain");
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
  myUtils.setDetectorFlavor("bemc");
  myUtils.readTablesFromASCII(dir,".txt");  
  myUtils.saveAbbreviatedStatusTablesToASCII(dir);  
  //CSMStatusUtils myUtils2;
  //myUtils2.setDetectorFlavor("eemc");
  //myUtils2.readTablesFromASCII(dir,".txt");  
  //myUtils2.saveAbbreviatedStatusTablesToASCII(dir);  
}
