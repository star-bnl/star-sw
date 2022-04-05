/** Macro to test FMS code on MuDST input. */

void runMudst(char* file="/star/u/akio/pwg/fms2015/mudst/st_fms_16077027_raw_4000001.MuDst.root", 
	      int ifile=0, Int_t nevt=1000, char* outdir="hist", int merge=1, int readMuDst=0, int print=0){  
  gROOT->Macro("load.C");  // Load all required libraries
  gSystem->Load("StEventMaker");

  StChain* chain = new StChain("StChain"); chain->SetDEBUG(0);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 1000, "MuDst");
  int n=muDstMaker->tree()->GetEntries();
  printf("Found %d entries in Mudst\n",n);
  int start=ifile*nevt;
  int stop=(ifile+1)*nevt-1;
  if(n<start) {printf(" No event left. Exiting\n"); return;}
  if(n<stop) {printf(" Overwring end event# stop=%d\n",n); stop=n;}
  printf("Doing Event=%d to %d\n",start,stop);

  StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
  filterMaker->printTriggerId();
  const int TIDBASE=480800;
  const int MAXVERSION=3;
  const int NTRG=11;  //123=FMS-sm-bs123,456=FMS-lg-bs123,7=FMS-DiBS,8910=FMS-JP012,11=FMS-DiJP,13=LED
  for(int i=1; i<=NTRG; i++){
    for(int j=0; j<MAXVERSION; j++){
      int id=TIDBASE + 20*j + i; 
      printf("Adding TriggerId=%d to the filter %d %d\n",id,i,j);
      filterMaker->addTrigger(id);
    }
  } 
  filterMaker->addVetoTrigger(480813); //Veto LED events
  filterMaker->addVetoTrigger(480833);

  St_db_Maker* dbMk = new St_db_Maker("db","MySQL:StarDb","$STAR/StarDb"); 
  dbMk->SetDEBUG(0); //dbMk->SetDateTime(20150301,0);

  StFmsDbMaker* fmsdb = new StFmsDbMaker("fmsDb");  
  fmsdb->setDebug(1); 
  //fmsdb->readGainFromText();
  fmsdb->readRecParamFromFile();

  StEventMaker* eventMk = new StEventMaker();
  StFmsHitMaker* fmshitMk = new StFmsHitMaker();
  StFmsPointMaker* fmsptMk = new StFmsPointMaker("StFmsPointMaker");;  
  fmsptMk->setMergeSmallToLarge(merge);
  if(readMuDst){
    fmshitMk->SetReadMuDst();
    fmsptMk->SetReadMuDst();
  }
  StFmsFpsMaker* fmsfps = new StFmsFpsMaker(); 
  fmsfps->setReadMuDST();
  fmsfps->setPrint(print);
  TString filename(file);
  filename.ReplaceAll("mudst",outdir);
  filename.ReplaceAll(".MuDst.root",Form(".%d.fmsfps.root",ifile));
  fmsfps->setQA(filename.Data());

  chain->Init();
  chain->EventLoop(start,stop);
  chain->Finish();
  delete chain;
}
