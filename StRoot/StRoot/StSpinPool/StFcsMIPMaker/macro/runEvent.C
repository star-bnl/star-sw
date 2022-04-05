void runEvent(Int_t nevents=10, Int_t pedLedPhy=2, Int_t eventDisplay=0, Int_t debug=0,
	    const Char_t *file="/gpfs01/star/subsysg/FPS/fcs2019/event/liangxl/gain0015_20192002/st_physics_20192002_raw_1500002.event.root"){

  cout<<" Input Filename = "<<file<<endl;
  TString opt = "in fcsCluster";
  
  TString f(file);
  int l1 = f.Length();
  int l2 = f.Last('/');
  TString dir(f);  dir.Remove(l2+1,l1-l2);
  TString fne(f);  fne.Remove(0,l2+1);
  TString crun(fne);
  int l3= crun.First('_'); crun.Remove(0,l3+1);
  int l4= crun.First('_'); crun.Remove(0,l4+1);
  TString srun(crun);
  int l5 =crun.Length(); 
  int l6= crun.First('_'); crun.Remove(l6,l5-l6);
  srun.Remove(0,l6+1);
  int l7=srun.First('_'); srun.Remove(0,l7+1);
  int l8 = srun.First(".");
  int l9 = srun.Length();
  srun.Remove(l8,l9-l8);

  int run=crun.Atoi();
  int subrun=srun.Atoi();
  int day=run/1000;    
  printf("Run=%d SubRun=%d yearday=%d\n",run,subrun,day);
  
  gROOT->LoadMacro("bfc.C");  // Load big "full" chain
  bfc(-1,opt,file);     // Setup but do not init
  gROOT->Macro("load.C");
  
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

    StFcsDbMaker *fcsDbMkr=(StFcsDbMaker *)chain->GetMaker("fcsDb");
    fcsDbMkr->setRun(run);
    fcsDbMkr->setDebug(debug);
    fcsDbMkr->setRun19(1);
    fcsDbMkr->readGainFromText();
    fcsDbMkr->readGainCorrFromText();

    Int_t filter=1;
    if(filter){
        gSystem->Load("StTriggerFilterMaker");
        StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
        filterMaker->printTriggerId();
        filterMaker->addTrigger(19);
        filterMaker->addTrigger(20);
        filterMaker->addTrigger(21);
        filterMaker->addTrigger(22);
        filterMaker->addTrigger(23);
        filterMaker->addTrigger(24);
        filterMaker->addTrigger(25);
        filterMaker->addTrigger(26);
        filterMaker->addTrigger(27);
        filterMaker->addTrigger(28);
        filterMaker->addTrigger(29);
        filterMaker->addTrigger(30);
        filterMaker->addTrigger(31);
        filterMaker->addTrigger(700001);
        filterMaker->addTrigger(700013);
    }



    //StFcsRawHitMaker *hitmk=(StFcsRawHitMaker *)chain->GetMaker("fcsHit");
    //hitmk->setDebug(debug);

    StFcsClusterMaker *clu=(StFcsClusterMaker *)chain->GetMaker("StFcsClusterMaker");
    /*    
    gSystem->Load("StFcsEventDisplay");
    StFcsEventDisplay* fcsed;
    if(pedLedPhy>0 && eventDisplay>0){
	fcsed = new StFcsEventDisplay();
	fcsed->setMaxEvents(eventDisplay);
	sprintf(edout,"%d/%d.eventDisplay.png",day,run);
	fcsed->setFileName(edout);
	fcsed->setFilter(1);
	fcsed->setRun19(1);
    }
    
    gSystem->Load("StFcsQaMaker");
    StFcsQaMaker *qaMkr=new StFcsQaMaker("FcsQa");  
    qaMkr->setRun(run);
    */
    gSystem->Load("StFcsMIPMaker");
    StFcsMIPMaker *MIPMkr=new StFcsMIPMaker("FcsMIP");  
    MIPMkr->setRun(run);
    MIPMkr->setSubRun(subrun);

    if(pedLedPhy==0){
	//hitmk->setReadMode(0);
	qaMkr->setMaxTimeBins(1024);
	qaMkr->setMaxAdc(512);      
    }else if(pedLedPhy==1){
	//hitmk->setReadMode(1);
	qaMkr->setMaxTimeBins(256);
	qaMkr->setSumTimeBins(210,225);  
	qaMkr->setMaxAdc(2048);      
	qaMkr->setMaxAdcSum(20000);      
	clu->setEnergySelect(1);
	clu->setSumTimeBins(210,225);
    }else if(pedLedPhy==2){
	//hitmk->setReadMode(1);
	//qaMkr->setMaxTimeBins(160);
	//qaMkr->setSumTimeBins(35,60);  
	//qaMkr->setMaxAdc(1024);      
	//qaMkr->setMaxAdcSum(2000);      
	clu->setEnergySelect(1);
	clu->setSumTimeBins(48,60);
    }

    // Intializet the chain
    chain->Init();

    // Process events
    chain->EventLoop(nevents);    
}
