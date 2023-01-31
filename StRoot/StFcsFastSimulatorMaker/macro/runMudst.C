void runMudst(char* file="st_cosmic_adc_22326042_raw_0000005.MuDst.root", 
	      int ifile=-1, Int_t nevt=-1, char* outdir=".", int readMuDst=1){  
    gROOT->Macro("Load.C");
    gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
    gSystem->Load("StEventMaker");
    gSystem->Load("StFcsDbMaker");
    gSystem->Load("StFcsRawHitMaker");
    gSystem->Load("StFcsWaveformFitMaker");
    gSystem->Load("StFcsClusterMaker");
    gSystem->Load("libMinuit");
    gSystem->Load("StFcsPointMaker");

    StChain* chain = new StChain("StChain"); chain->SetDEBUG(0);
    StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 1000, "MuDst");
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
    StEventMaker* eventMk = new StEventMaker();
    StFcsRawHitMaker* hit = new StFcsRawHitMaker();  
    hit->setReadMuDst(readMuDst);
    StFcsWaveformFitMaker *wff= new StFcsWaveformFitMaker();
    StFcsClusterMaker *clu= new StFcsClusterMaker();
    StFcsPointMaker *poi= new StFcsPointMaker();
    wff->SetDebug();
    clu->SetDebug();
    poi->SetDebug();

    chain->Init();
    chain->EventLoop(start,stop);
    chain->Finish();
    delete chain;
}
