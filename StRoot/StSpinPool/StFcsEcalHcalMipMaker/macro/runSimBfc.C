TString input_dir   = "./";
TString output_dir  = "./";
TString input_chain = "sdt20211025.120000,fzin,geant,FieldOn,logger,MakeEvent,CMuDst,fcsSim,fcsWFF,fcsCluster,fcsPoint";

class StFmsSimulatorMaker;

void runSimBfc( Int_t nEvents=1000, Int_t run=1, const char* pid="mu+", int TrgVersion=202207,
		int debug=0, int e=0, float pt=4.0, float vz=0.0,
		char* epdmask="0.0300",
		int leakyHcal=0, 
		int eventDisplay=10,
		TString myDir=input_dir, TString myOutDir=output_dir,
		TString myChain=input_chain, Int_t mnEvents=0){
    
  gROOT->LoadMacro("bfc.C");
  //  gROOT->Macro("loadMuDst.C");
  TString myDat;
  TString proc(pid);
  if(proc.Contains("dy") || proc.Contains("mb") || proc.Contains("jet") || proc.Contains("dybg")){
      myDat=Form("pythia.%s.vz%d.run%i.fzd",pid,(int)vz,run);
  }else if(e>0.0){
      myDat=Form("%s.e%d.vz%d.run%i.fzd",pid,e,(int)vz,run);
  }else{
      myDat=Form("%s.pt%3.1f.vz%d.run%i.fzd",pid,pt,(int)vz,run);
  }
  printf("Opening %s\n",(myDir+myDat).Data());
  bfc( -1, myChain, myDir+myDat );
 
  TString outfile = myOutDir + myDat.ReplaceAll(".fzd",".root");      
  cout << "output file=" <<outfile<<endl;
  chain->SetOutputFile(outfile);
  
  St_db_Maker *dbMk= (St_db_Maker*) chain->GetMaker("db");
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
  dbMk->SetAttr("blacklist", "Calibrations_rich");

  StFcsDbMaker* fcsdbmkr = (StFcsDbMaker*) chain->GetMaker("fcsDbMkr");  
  cout << "fcsdbmkr="<<fcsdbmkr<<endl;

  StFcsDb* fcsdb = (StFcsDb*) chain->GetDataSet("fcsDb");  
  cout << "fcsdb="<<fcsdb<<endl;
  fcsdb->forceUniformGain(0.0053);
  fcsdb->forceUniformGainCorrection(1.0);

  StFcsFastSimulatorMaker *fcssim = (StFcsFastSimulatorMaker*) chain->GetMaker("fcsSim");

  StFcsWaveformFitMaker *wff=(StFcsWaveformFitMaker *)chain->GetMaker("StFcsWaveformFitMaker");
  wff->setEnergySelect(0);
  wff->setDebug(debug);

  StFcsClusterMaker *clu=(StFcsClusterMaker *)chain->GetMaker("StFcsClusterMaker");
  clu->setDebug(debug);

  StFcsPointMaker *poi=(StFcsPointMaker *)chain->GetMaker("StFcsPointMaker");
  poi->setDebug(debug);
  poi->setShowerShape(3);
  
  gSystem->Load("StFcsEcalHcalMipMaker");
  StFcsEcalHcalMipMaker* mip = new StFcsEcalHcalMipMaker;
  mip->setRun(run);

  /*
  gSystem->Load("RTS");
  gSystem->Load("StFcsTriggerSimMaker");
  StFcsTriggerSimMaker* fcsTrgSim = new StFcsTriggerSimMaker(); 
  fcsTrgSim->setSimMode(1);
  fcsTrgSim->setTrigger(TrgVersion);
  fcsTrgSim->setDebug(debug);
  fcsTrgSim->setEtGain(1.0); //ET match
  //fcsTrgSim->setEtGain(0.5); //halfway
  //fcsTrgSim->setEtGain(0.0); //E match
  //fcsTrgSim->setReadPresMask(Form("mask/fcs_ecal_epd_mask.ele.pt0.6.vz0.thr%s.txt",epdmask));
  //TString txfile(outfile); txfile.ReplaceAll(".root",".event.txt");  fcsTrgSim->setWriteEventText(txfile.Data());
  TString qafile(outfile); qafile.ReplaceAll(".root",".qahist.root"); fcsTrgSim->setWriteQaHist(qafile.Data());
  fcsTrgSim->setThresholdFile("stage_params.txt");

  gSystem->Load("StFcsTrgQaMaker");
  StFcsTrgQaMaker* fcsTrgQa = new StFcsTrgQaMaker(); 
  TString tqafile(outfile); tqafile.ReplaceAll(".root",Form(".thr%s.trgqa.root",epdmask)); 
  fcsTrgQa->setFilename(tqafile.Data());
  fcsTrgQa->setEcalPtThr(pt*0.75);
  */

  if(eventDisplay>0){
    gSystem->Load("StEpdUtil");
    gSystem->Load("StFcsEventDisplay");
    StFcsEventDisplay* fcsed = new StFcsEventDisplay();
    fcsed->setMaxEvents(eventDisplay);
    outfile.ReplaceAll(".root",".eventDisplay.png");
    fcsed->setFileName(outfile.Data());
  }

  chain->Init();
  StMaker::lsMakers(chain);
  chain->EventLoop(mnEvents,nEvents);  
  chain->Finish(); 
}
