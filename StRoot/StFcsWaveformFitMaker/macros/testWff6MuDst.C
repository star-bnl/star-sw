/*
  Orig Author: Akio Ogawa
  Edited: David Kapukchyan
  @[May 18, 2022](David Kapukchyan)
  > Copied from $STAR/StRoot/StFcsFastSimulatorMaker/macros/runMudst.C and modified so that I can test my own StFcsWaveformFitMaker.
  
  Macro to run micro dst from fast production of FCS data for Run 22 data.
*/

void testWff(int nevt = 1,
	     const char* outdir="test.root",
	     const char* file="st_fwd_23080044_raw_1000019.MuDst.root",
	     int readMuDst=1
	     )
{
  gROOT->Macro("Load.C");
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gSystem->Load("StEventMaker");
  gSystem->Load("StFcsDbMaker");
  gSystem->Load("StFcsRawHitMaker");
  gSystem->Load("StFcsWaveformFitMaker");
  gSystem->Load("libMinuit");
    
  //gMessMgr->SetLimit("I", 0);
  //gMessMgr->SetLimit("Q", 0);
  //gMessMgr->SetLimit("W", 0);
  
  gStyle->SetOptDate(0);
  gStyle->SetOptFit(0);

  StChain* chain = new StChain("StChain"); chain->SetDEBUG(0);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 1000, "MuDst");
  int n=muDstMaker->tree()->GetEntries();
  printf("Found %d entries in Mudst\n",n);
    
  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb"); 
  if(dbMk){
    dbMk->SetAttr("blacklist", "tpc");
    dbMk->SetAttr("blacklist", "svt");
    dbMk->SetAttr("blacklist", "ssd");
    dbMk->SetAttr("blacklist", "ist");
    dbMk->SetAttr("blacklist", "pxl");
    dbMk->SetAttr("blacklist", "pp2pp");
    dbMk->SetAttr("blacklist", "ftpc");
    dbMk->SetAttr("blacklist", "emc");
    dbMk->SetAttr("blacklist", "eemc");
    dbMk->SetAttr("blacklist", "mtd");
    dbMk->SetAttr("blacklist", "pmd");
    dbMk->SetAttr("blacklist", "tof");
    dbMk->SetAttr("blacklist", "etof");
    dbMk->SetAttr("blacklist", "rhicf");
  }
  
  StFcsDbMaker *fcsDbMkr= new StFcsDbMaker();
  StFcsDb* fcsDb = (StFcsDb*) chain->GetDataSet("fcsDb");
  fcsDb->setReadGainCorrFromText();
  StFcsDbPulse* fcsDbPulse = (StFcsDbPulse*) chain->GetDataSet("fcsPulse");
  StEventMaker* eventMk = new StEventMaker();
  StFcsRawHitMaker* hit = new StFcsRawHitMaker();
  //hit->setDebug();
  hit->setReadMuDst(readMuDst);
  StFcsWaveformFitMaker *wff= new StFcsWaveformFitMaker();
  wff->writeFile(outdir);
  wff->setTest(6);//Sets energy select to proper value if not set already
  wff->setEnergySumScale(1.0,1.0,1.0);
  //wff->setMeasureTime("measure.png"); //Turn on if you want to measure the time for fitting
  //wff->SetDebug();                    //Turn on for debug output
  
  chain->Init();

  TStopwatch clock;
  //Event loop
  for( UInt_t i=0; i<nevt; ++i ){
    chain->Make();
    chain->Clear();
  }
  std::cout << "========================================" << std::endl;
  std::cout << clock.RealTime() << " seconds" << std::endl;
  std::cout << "========================================" << std::endl;

  chain->Finish();
  delete chain;
}

