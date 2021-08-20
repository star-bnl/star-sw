TString opt = "in MakeEvent Mudst evout trgd fcsDat fcsWFF fcsCluster fcsPoint Tpcdb";

void runBfc(Int_t nevents=1000000, const Char_t *file="st_fcs_22179052_raw_0000001.daq", Int_t pedLedPhy=3, Int_t eventDisplay=1000, Int_t debug=0){

    TString f(file);
    int l1 = f.Length();
    int l2 = f.Last('/');
    TString dir(f);  dir.Remove(l2+1,l1-l2);
    TString fne(f);  fne.Remove(0,l2+1);
    TString crun(fne);
    int l3= crun.First('_'); crun.Remove(0,l3+1);
    int l4= crun.First('_'); crun.Remove(0,l4+1);
    int l5 =crun.Length(); int l6= crun.First('_'); crun.Remove(l6,l5-l6);
    int run=crun.Atoi();
    int day=run/1000;    
    //printf("l1=%d l2=%d l3=%d l4=%d l5=%d l6=%d\n",l1,l2,l3,l4,l5,l6);
    //printf("f  =%s\n",f.Data());
    //printf("dir=%s\n",dir.Data());
    //printf("fne=%s\n",fne.Data());
    //printf("run=%s\n",run.Data());
    printf("Run=%d yearday=%d\n",run,day);
    char edout[200];

    gROOT->LoadMacro("bfc.C");  // Load big "full" chain
    bfc(-1,opt,file);     // Setup but do not init
        
    St_db_Maker *dbMk= (St_db_Maker*) chain->GetMaker("db");
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

    StFcsDbMaker* fcsDbMkr = (StFcsDbMaker*) chain->GetMaker("fcsDbMkr");
    //cout << "fcsDbMkr="<<fcsDbMkr<<endl;

    StFcsDb* fcsDb = (StFcsDb*) chain->GetDataSet("fcsDb");
    //cout << "fcsDb="<<fcsDb<<endl;
    fcsDb->setDbAccess(0);
    fcsDb->setDebug(debug);
    //    fcsDb->forceUniformGain(0.0053);
    fcsDb->forceUniformGain(0.0053*2.0); //tail has 1/2 integral
    fcsDb->forceUniformGainCorrection(1.0);
    //fcsDb->setRun(run);
    //fcsDb->setRun19(0);
    //fcsDb->readGainFromText();
    //fcsDb->readGainCorrFromText();

    Int_t filter=0;
    if(filter){
      gSystem->Load("StTriggerFilterMaker");
      StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
      filterMaker->printTriggerId();
      filterMaker->addTrigger(860000);  //mb-hlt-200
      //filterMaker->addTrigger(860009);  //mb-hlt-30
      filterMaker->addTofUpperLimit(20);
    }

    StFcsRawHitMaker *hitmk=(StFcsRawHitMaker *)chain->GetMaker("fcsHit");
    hitmk->setDebug(debug);

    StFcsWaveformFitMaker *wff=(StFcsWaveformFitMaker *)chain->GetMaker("StFcsWaveformFitMaker");
    wff->setDebug(debug);
    wff->setEnergySelect(10);    
    wff->setTail(2);
    char wffout[100];
    char wffout2[100];
    if(pedLedPhy==3){ //cosmic
      wff->setEnergySelect(2);
      wff->setCenterTimeBins(50,30,100);
    }else if(pedLedPhy==1){ //LED
      if(run<1106734){
	wff->setCenterTimeBins(207,190,240);
      }else{
	wff->setCenterTimeBins(207+8,190+8,240+8);
      }      
      wff->setPedTimeBins(0,150);
      skip=5;
    }else{ //Physics
      wff->setCenterTimeBins(50,30,100);
    }
    sprintf(wffout,"%d/%d.wff",day,run);
    wff->setFileName(wffout,10,0);
    sprintf(wffout2,"%d/%d.wfftime.png",day,run);
    wff->setMeasureTime(wffout2);    

    StFcsClusterMaker *clu=(StFcsClusterMaker *)chain->GetMaker("StFcsClusterMaker");
    clu->setDebug(debug);
    if(pedLedPhy==3){ //cosmic
      clu->sortById();
      clu->setNeighborDistance(2.4,2.4);
      clu->setTowerEThreshold(0.001,0.001);
      clu->setTowerEThreMoment(0.001,0.001);
      clu->setTowerERatio2Split(100,100);
    }

    if(pedLedPhy==2){ //physics
      StFcsPointMaker *poi=(StFcsPointMaker *)chain->GetMaker("StFcsPointMaker");
      poi->setDebug(debug);
      poi->setShowerShape(3);
    }

    if(pedLedPhy==3){ //cosmic
      gSystem->Load("StFcsCosmicMaker");
      StFcsCosmicMaker* cosmic = new StFcsCosmicMaker();
      cosmic->setRun(run);
    }

    gSystem->Load("StFcsEventDisplay");
    StFcsEventDisplay* fcsed;
    if(pedLedPhy>0 && eventDisplay>0){
        gSystem->Load("StEpdUtil");
	fcsed = new StFcsEventDisplay();
	fcsed->setMaxEvents(eventDisplay);
	sprintf(edout,"%d/%d.eventDisplay.png",day,run);
	fcsed->setFileName(edout);
	fcsed->setFilter(1);
	if(pedLedPhy==3) fcsed->setMinMax(-3,1);
    }

    gSystem->Load("StFcsQaMaker");
    StFcsQaMaker *qaMkr=new StFcsQaMaker("FcsQa");  
    qaMkr->setRun(run);

    if(pedLedPhy==0){
	hitmk->setReadMode(0);
	qaMkr->setMaxTimeBins(1024);
	qaMkr->setMaxAdc(512);      
    }else if(pedLedPhy==1){
	hitmk->setReadMode(0);
	qaMkr->setMaxTimeBins(256);
	qaMkr->setSumTimeBins(210,225);  
	qaMkr->setMaxAdc(2048);      
	qaMkr->setMaxAdcSum(20000);      
    }else if(pedLedPhy==2){
	hitmk->setReadMode(1);
	qaMkr->setMaxTimeBins(160);
	qaMkr->setSumTimeBins(35,60);  
	qaMkr->setMaxAdc(1024);      
	qaMkr->setMaxAdcSum(2000);      
    }
    
    // Intializet the chain
    chain->Init();

    // Process events
    chain->EventLoop(nevents);    
}
