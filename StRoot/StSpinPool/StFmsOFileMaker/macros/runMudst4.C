/* Execute MuDST-->OFile maker: StFmsOFileMaker */

/* arguments of runMudst4: 
 * - file -- muDST root file
 * - jobid -- unique id for this job
 * - ifile -- just leave as -1
 * - nevt -- number of events to read (set to -2 to read all of them)
 */

//void runMudst4(char* file="st_fms_16069001_raw_2500015.MuDst.root",Int_t runnum=16069001,
void runMudst4(char* file="st_fms_16088020_raw_5500016.MuDst.root",Int_t runnum=16088020,
               char * jobid="single",
               int ifile=-1,
               Int_t nevt=10000 
              ) {  

  // load libraries
  loadLibs();

  // instantiate muDstMaker ==========================================================

  // chain and mudst maker
  StChain* chain = new StChain("StChain"); chain->SetDEBUG(0);
  printf("--------------------------- error below here\n");
  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 3000, "MuDst");
  // StMuDstMaker constructor arguments
  //  -- int mode -- 0=read; 1=write
  //  -- int nameMode -- 0=read from inFile; 1=ioMaker
  //  -- char * dirName -- 
  //  -- char * fileName -- name of .root or filelist (pass to macro)
  //  -- char * filter -- 
  //  -- int maxFiles -- max # of root files to chain
  //  -- char * name -- 
  printf("--------------------------- error above here\n");

  // figure out how many events to read
  int n=muDstMaker->tree()->GetEntries();
  printf("Found %d entries in Mudst\n",n);
  int start=0, stop=n;
  if(ifile>=0){
    int start=ifile*nevt;
    int stop=(ifile+1)*nevt-1;
    if(n<start) {printf(" No event left. Exiting\n"); return;}
    if(n<stop)  {printf(" Overwriting end event# stop=%d\n",n); stop=n;}
  }else if(nevt>=0 && nevt<n){
    stop=nevt;
  }else if(nevt==-2){
    stop=2000000000; 
  }
  printf("Doing Event=%d to %d\n",start,stop);




  // DB makers ===========================================================================
  
  // StarDB
  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb"); 
	dbMk->SetAttr("blacklist", "eemc");
	dbMk->SetAttr("blacklist", "ftpc");
	dbMk->SetAttr("blacklist", "ist");
	dbMk->SetAttr("blacklist", "mtd");
	dbMk->SetAttr("blacklist", "pmd");
	//dbMk->SetAttr("blacklist", "pp2pp");
	dbMk->SetAttr("blacklist", "pxl");
	dbMk->SetAttr("blacklist", "ssd");
	dbMk->SetAttr("blacklist", "svt");
	dbMk->SetAttr("blacklist", "tof");
	dbMk->SetAttr("blacklist", "tpc");
	dbMk->SetDebug();
  //dbMk->SetDateTime(20150301,0);

  // FMS DB
  StFmsDbMaker* fmsDB = new StFmsDbMaker("fmsDb");
  fmsDB->setDebug(0);
  //fmsDB->readGainFromText();
  //fmsDB->readRecParamFromFile();
  
  // spin DB
  StSpinDbMaker * spinDB = new StSpinDbMaker("spinDb");
  //spinDB->InitRun(runnum);
  

  // TPC DB (needed by StDAQMaker?)
  //StTpcDbMaker * tpcDB = new StTpcDbMaker("tpcDb");
  
  // for accessing StDAQReader
  //StDAQMaker * daqMk = new StDAQMaker("daqMk",file);


  

  // event makers =========================================================================

  // event 
  StEventMaker * eventMk = new StEventMaker();
  //eventMk->doPrintEventInfo = true; // verbose event output
  
  // trigger data
  StTriggerDataMaker * triggerMk = new StTriggerDataMaker();
  triggerMk->setDebug(1);


  // FMS
  //StFmsTriggerMaker * fmsTrigMk = new StFmsTriggerMaker(); // doesn't work; needs old sql DB access
  StFmsHitMaker * fmsHitMk = new StFmsHitMaker();
  StFmsPointMaker * fmsPointMk = new StFmsPointMaker();


  // FPS
  StFmsFpsMaker * fpsMk = new StFmsFpsMaker();


  // Instantiate old OFile maker, which used StMuEvent for FMS (insteaed of StEvent); DEPRECATED
  //gSystem->Load("StMuFmsAnalysisMaker");
  //StMuFmsAnalysisMaker* ofileMk = new StMuFmsAnalysisMaker(muDstMaker); 


  // OFile Maker
  StFmsOFileMaker * ofileMk = new StFmsOFileMaker(muDstMaker);



  //////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////
  // BEGIN SET MAKER OPTIONS
  //
  // NOTE: listing 'default' values for batch OFile production in "( )" 

  // mudst reading
  // if 0, get info from StTriggerData from StTriggerDataMaker/StEvent/MuDst
  // and apply new DB; cluster finding/fitting is redone
  // if 1, cluster finding/fitting is NOT redone (i.e., mudst result is read
  // out); however, some recalculations (like point positions) from updated DB values
  // are done
  fmsPointMk->SetReadMuDst(0); // (0)
  fmsHitMk->SetReadMuDst(0); // (0)
  fpsMk->setReadMuDST(1); // (1) // (reverse 0<->1 convention as FmsHitMaker and FmsPointMaker)

  // shower shape
  // 0='use lednev params', 1='use zhanwen params', 2='use yuxi params'
  fmsPointMk->setShowerShapeWithAngle(1); // (1)
  
  // cluster merging over large/small boundary
  // if 1, merges large and small cell clusters (experimental!; default 1 in StFmsPointMaker)
  fmsPointMk->setMergeSmallToLarge(1); // (1)

  // vertex correction
  // if 0, no vertex correction; if 1, use MuDst BBC vertex based on run11 calibration (needs update?)
  fmsPointMk->setVertexZ(1); // (1)

  // output verbosity of analysis maker
  ofileMk->verbose = false; // (false) // if true, prints extra output from analysis maker
  ofileMk->verbose_rp = false; // (false) // if true, prints extra output specific to RP events
  ofileMk->dump_spinbits = true; // (true) // if true, dumps spin patterns during first event output

  // do spin QA (if set to true)
  ofileMk->check_spinbyte = false; // (false)

  // build extra trees into MFiles
  ofileMk->build_evtr = false; // (false) // if true, builds easier-to-read-but-bigger-than-p_out event tree 


  // END SET MAKER OPTIONS
  //////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////




  // define OFile maker outputs
  TString filenameOFileMaker(file);
  TRegexp re("\/.*\/");
  filenameOFileMaker(re) = "";
  filenameOFileMaker.ReplaceAll("sched",Form("%d.",runnum));
  filenameOFileMaker.ReplaceAll("list","fmsan.root");
  filenameOFileMaker.ReplaceAll("MuDst","fmsan");

  cout << "StFmsOFileMaker outfile name = " << filenameOFileMaker.Data()<<endl;
  ofileMk->setFileName(filenameOFileMaker.Data());
  ofileMk->setPrint(1);



  // execute chain
  chain->Init();
  chain->EventLoop(start,stop);
  chain->Finish();
  delete chain;
}


// load libraries
void loadLibs() {
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
  gSystem->Load("StDbLib");
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
  gSystem->Load("StTriggerDataMaker");
  //gSystem->Load("StDAQMaker");
  gSystem->Load("libMinuit.so");
  gSystem->Load("StFmsUtil");
  gSystem->Load("StFmsDbMaker");
  gSystem->Load("StFmsHitMaker");
  gSystem->Load("StFmsPointMaker");
  gSystem->Load("StFmsTriggerMaker");
  gSystem->Load("StFmsFpsMaker");
  gSystem->Load("StMuRpsUtil.so");

  gSystem->Load("StFmsOFileMaker");
}; // end loadLibs()
