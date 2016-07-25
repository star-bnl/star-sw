//======================================================================
//
////////////////////////////////////////////////////////////////////////
//
//
//
// Macro to perform the calibration of the TPC
//
//
//
////////////////////////////////////////////////////////////////////////
//

Int_t NoEvents = 0;

//_____________________________________________________________________

void kai(const char* aFileName="/star/simu/simu/gstardata/rcf1201_57_100evts.fzd",  int aNEvent=1) {
 
  TString Chain("COMPLETE,simu,fzin,tables,-emc_T,-ftpcT,db,tpc,trs,svt,srs,Kalman,global,Primary,Vertex,event,McEvent, GeantOut evout");
  gROOT->LoadMacro("bfc.C");
  bfc(-1,Chain.Data(),aFileName);
  St_db_Maker *dbMk = chain->Maker("db");
  dbMk->SetDateTime(20010501,000000); 
  St_db_Maker *dbaseMk = new St_db_Maker("svt","MySQL:StarDb","$STAR/StarDb"); 
  dbaseMk-> SetDateTime(20010501,000000); 
// ______________ _________________________________________
//                  Run the chain 
// 
  Int_t iInitBad=chain->Init(); 
  chain->InitRun(0); 
  int NEvents = aNEvent; 
  if(!iInitBad){ 
    int iMakeBad=0; 
    int iEvt=1; 
  EventLoop: if (iEvt <= NEvents){// && !iMakeBad) { 
      cout << "Process event " << iEvt << endl; 
      chain->Clear(); 
      iMakeBad = chain->Make(); 
      if (iMakeBad) { 
	cout << "Should be stoping, Make return following status :" << iMakeBad << endl;
	
      } 
      iEvt++; 
      goto EventLoop; 
    } 
    //Int_t iFinish=chain->Finish(); 
  } 
} 




