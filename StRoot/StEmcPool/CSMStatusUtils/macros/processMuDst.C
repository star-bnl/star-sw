void processMuDst( //Char_t *jobId= "7121003.",
		  Char_t *jobId= "7129031.",
                 Char_t *dir="",
		   //Char_t *file="/star/data35/reco/productionPP/ReversedFullField/DEV/2004/117/st_physics_5117072_raw_*.MuDst.root",
		   //Char_t *file="/star/data54/reco/ppProductionTrans/FullField/P06ie/2006/129/7129031/st_physics_7129031_raw_1030003.MuDst.root",
		  Char_t *file="/star/data52/reco/ppProductionTrans/FullField/P06ie/2006/129/7129027/st_physics_adc_7129027_raw_1060001.MuDst.root",
                 Char_t *scratchDir ="/star/u/staszak/working/bemc/") {
  
  Int_t numberOfFilesInt = 1;
  

  cout << "RUNNING job " << jobId << ": reading " << numberOfFilesInt << " and writing output to "
       << scratchDir << " - dir: " << dir  << " - file: " << file << endl;
  
  // load shared libraries
  // first MuDst libs.
  if (gClassTable->GetID("TTable") < 0) {
	 gSystem->Load("libStar");
	 gSystem->Load("libPhysics");
  }
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  // load db libraries
  gSystem->Load("StarMagField");
    gSystem->Load("StMagF");
    gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
    gSystem->Load("StDaqLib");
  gSystem->Load("StDbLib");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker"); 
  gSystem->Load("StEEmcDbMaker"); 
  // load EMC libraries
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  // mc stuff since i use it in the library
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StAssociationMaker");
  
  // load my analysis library
  //gSystem->Load("StElectronInvMassAna");
  gSystem->Load("StEmcPoolCSMStatusUtils");  

  StChain *myChain = new StChain("myChain");
  myChain->SetDebug(0);
  StMuDebug::setLevel(0);
  StMuDstMaker *muDstMaker = new StMuDstMaker(0,0,dir,file,"",numberOfFilesInt,"MuDst");

  St_db_Maker *dbMaker = new St_db_Maker("StarDb","MySQL:StarDb");
  StEmcADCtoEMaker *adc2EMaker = new StEmcADCtoEMaker();
  adc2EMaker->setPrint(kFALSE);
  adc2EMaker->saveAllStEvent(kTRUE);

  StEEmcDbMaker *eemcDbMaker = new StEEmcDbMaker("eemcDbMaker");
  
  StBemcStatusMaker* myBemcStatusMaker = new StBemcStatusMaker(muDstMaker);
  myBemcStatusMaker->setOutputDirectory(scratchDir);
  myBemcStatusMaker->setOutputFilePrefix(jobId);
  
  myChain->PrintInfo();
  
  if(myChain->Init() != 0) {
    cerr << "Failure during Init()!" << endl;
  }
  Int_t iRet = 0;
  Int_t runNumber = -1;
  Int_t nEventsAna = 0;
  while (iRet == 0) {
    myChain->Clear();
    // we can't use the chain->Make function here since we need to take
    // care of StMuDstMaker
    iRet = myChain->Make(nEventsAna);
    if (iRet != 0) {
      cout << "rteutrn valuer snons ZEROOOOOOOOOOOOOOOOOOOOOOOOOOOOO" << endl;
      
      break;
    }
    if (nEventsAna % 100 == 0) {
      cout << "Analyzing event " << nEventsAna << endl;
    }
    nEventsAna++;
    if (nEventsAna==20000) break;
  }
  cout << "Analysed " << nEventsAna << " events" << endl;
  myChain->Finish();
  delete myChain;
}
