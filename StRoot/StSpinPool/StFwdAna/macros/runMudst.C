void runMudst(char* file="st_fwd_23080044_raw_1000019.MuDst.root", 
	      int ifile=-1, Int_t nevt=10, char* outdir=".", int readMuDst=1, int debug=0){  
  gROOT->Macro("Load.C");
  gROOT->Macro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");  
  gSystem->Load("StEventMaker");
  gSystem->Load("StFcsDbMaker");
  gSystem->Load("StEpdDbMaker");
  gSystem->Load("StEpdUtil");
  gSystem->Load("StEpdHitMaker");
  gSystem->Load("StFcsRawHitMaker");
  gSystem->Load("StFcsWaveformFitMaker");
  gSystem->Load("StFcsClusterMaker");
  gSystem->Load("libMinuit");
  gSystem->Load("StFcsPointMaker");
    
  gMessMgr->SetLimit("I", 0);
  gMessMgr->SetLimit("Q", 0);
  gMessMgr->SetLimit("W", 0);

  StChain* chain = new StChain("StChain"); chain->SetDEBUG(0);
  StMuDstMaker* muDstMaker = new StMuDstMaker(0, 0, "", file,".", 1000, "MuDst");
  int n=muDstMaker->tree()->GetEntriesFast();
  printf("Found %d entries in Mudst\n",n);
  int start=0;
  int stop=n;
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
  /*
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
    }*/

  gSystem->Load("StSpinDbMaker");
  StSpinDbMaker* spindb = new StSpinDbMaker("spinDb");
    
  StFcsDbMaker *fcsDbMkr= new StFcsDbMaker();
  StFcsDb* fcsDb = (StFcsDb*) chain->GetDataSet("fcsDb");
  //fcsDb->setReadGainFromText();
  //fcsDb->setReadGainCorrFromText();
  StEpdDbMaker* epddb = new StEpdDbMaker();
  
  StEventMaker* eventMk = new StEventMaker();
  
  StEpdHitMaker* epdhitmkr = new StEpdHitMaker();
  epdhitmkr->setReadMuDst();
  StFcsRawHitMaker* hit = new StFcsRawHitMaker();
  hit->setReadMuDst(readMuDst);
  //StFcsWaveformFitMaker *wff= new StFcsWaveformFitMaker();
  //wff->setEnergySelect(13,13,1);
  //wff->setMaxPeak(8);
  //wff->SetDebug(debug);
  //wff->setAnaWaveform(false);
  //StFcsClusterMaker *clu= new StFcsClusterMaker();
  //StFcsPointMaker *poi= new StFcsPointMaker();
  //clu->SetDebug(debug);
  //poi->SetDebug(debug);

  //gSystem->Load("StVpdCalibMaker");
  //StVpdCalibMaker *vpdCalib = new StVpdCalibMaker();
  //vpdCalib->setMuDstIn();
    
  gSystem->Load("StFwdData");
  TString foriternum(file);
  Ssiz_t last_ = foriternum.Last('_');
  TString filename = "StFcsRun22Qa_";
  TString iternum = foriternum(last_+1,7);
  iternum.ReplaceAll(".list","");  //For the case a list file was the input then the iteration is no longer 7 digits but 1 or 2 which happens to be the perfect size to fit the ".list" extension. This line removes that extension.
  TString runnum;
  if( foriternum.Contains("_raw_") ){ runnum = foriternum(last_-12,12); } //if name contains _raw_ then keep that in the output file
  else{ runnum = foriternum(last_-8,8); }  //Since I know file name will have the form xrd_runnum_iternum.MuDst.root I can use this this hack to get the runnumber
  //filename += runnum + "_" + iternum + ".root";
  //std::cout << iternum.Atoi() << std::endl;
  //std::cout << "Suffix:"<<runnum << std::endl;
  //std::cout << "|Filename:" << filename << std::endl;
  TString filenametree = "FcsRun22Pi0Ana_";
  filenametree += runnum + "_" + iternum + ".root";  
  HistManager* treehists = new HistManager();
  treehists->InitFile(filenametree.Data(),"RECREATE");
  
  gSystem->Load("StFwdAna");
  StFwdAnaData* fwdanadata = new StFwdAnaData();
  fwdanadata->setTreeOnBit(0);
  fwdanadata->setRandomSeed(time(0));
  fwdanadata->setEpdNmipCut(0.7);
  //fwdanadata->setIgnoreTrig();
  //Below is list of all triggers for FCS Run 22
  fwdanadata->AddTrig("fcsJPsi");
  fwdanadata->AddTrig("fcsJPDE1");
  fwdanadata->AddTrig("fcsJPDE0");
  fwdanadata->AddTrig("fcsJPBC1");
  fwdanadata->AddTrig("fcsJPBC0");
  fwdanadata->AddTrig("fcsJPA1");
  fwdanadata->AddTrig("fcsJPA0");
  fwdanadata->AddTrig("fcsJP2");
  fwdanadata->AddTrig("fcsEM0");
  fwdanadata->AddTrig("fcsEM1");
  fwdanadata->AddTrig("fcsEM2");
  fwdanadata->AddTrig("fcsEM3");
  fwdanadata->AddTrig("fcsEM0_tpc");
  fwdanadata->AddTrig("fcsEM1_tpc");
  fwdanadata->AddTrig("fcsEM2_tpc");
  fwdanadata->AddTrig("fcsEM3_tpc");
  fwdanadata->AddTrig("fcsEHT-N/S");
  fwdanadata->AddTrig("fcsDYAsy");
  fwdanadata->AddTrig("fcsDY");
  fwdanadata->AddTrig("fcsDiJPAsy");
  fwdanadata->AddTrig("fcsDiJP");
  //Don't look at hadron triggers for pi0 analysis
  //fwdanadata->AddTrig("fcsHad0");
  //fwdanadata->AddTrig("fcsHad1");
  //fwdanadata->AddTrig("fcsHad2");
    
  StFwdAnaDataMaker* fwddatamkr = new StFwdAnaDataMaker();
  fwddatamkr->setAnaData(fwdanadata);
  fwddatamkr->setPolDataFilename("Run22PolForJobs.txt");
  fwddatamkr->setFcsTrigFilename("FcsSortedTrig.txt");
  //fwddatamkr->setOutFilename(filenametree.Data();//Only do this if an external hist manager was not declared and the file was not initialized like above 
  fwddatamkr->setHistManager(treehists);
  
  fwddatamkr->addAna(new StFwdAnaPolarization());
  fwddatamkr->addAna(new StFwdAnaSpin());
  
  StFwdAnaFcsRun22Qa* fcsqa = new StFwdAnaFcsRun22Qa();
  fcsqa->setFcsAdcTbOn(false);
  fcsqa->setEpdAdcQaOn(false);
  fcsqa->setEpdTacQaOn(false);
  fcsqa->setBestMassOn(false);
  fwddatamkr->addAna(fcsqa);
  //fwddatamkr->addAna(new StMuFcsAnaCheckFillClusPoint());  //This is for checking mudst clusters/points against clustermaker/pointmaker
  
  StFwdAnaEpdQaAndVert* epdqa = new StFwdAnaEpdQaAndVert();
  epdqa->setEpdTacAdcOn(false);
  fwddatamkr->addAna(epdqa);
    
  fwddatamkr->addAna(new StFwdAnaVertex());
  fwddatamkr->addAna(new StFwdAnaCheckTrig());
  fwddatamkr->addAna(new StFwdAnaFillEcalClusPoint());
  fwddatamkr->addAna(new StFwdAnaEpdMatch());
  StFwdAnaEpdMatchQa* matchqa = new StFwdAnaEpdMatchQa();
  matchqa->setCanvas(0);
  fwddatamkr->addAna(matchqa);
  fwddatamkr->addAna(new StFwdAnaEpdFcsMixedEvent());
  StFwdAnaMakeEcalPairs* makepairs = new StFwdAnaMakeEcalPairs();
  //makepairs->setMakeClusPairs(false);
  //makepairs->setMakePointPairs(false);
  fwddatamkr->addAna(makepairs);
  StFwdAnaEcalPairQa* pairqa = new StFwdAnaEcalPairQa();
  fwddatamkr->addAna(pairqa);
  fwddatamkr->addAna(new StFwdAnaEcalPi0Tssa());
    
  chain->Init();
  chain->EventLoop(start,stop);
  chain->Finish();
  delete chain;
  delete treehists;
}

