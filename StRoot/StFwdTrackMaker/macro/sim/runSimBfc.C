TString input_dir   = "./fcs2022";
TString output_dir  = ".";
TString input_chain = "sdt20211016.120000,dev2021,fzin,geant,FieldOn,logger,MakeEvent,ReverseField,agml,usexgeom,bigbig,fstFastSim,fttFastSim,fcsSim,fcsWFF,fcsCluster,fwdTrack";

void runSimBfc( Int_t nEvents=1000, Int_t run=1, const char* pid="JPsi", float vz=0.0,
		TString myDir=input_dir, TString myOutDir=output_dir,
		int TrgVersion=202207,
		int debug=0, int e=0, float pt=1.5,
		char* epdmask="0.0100",
		int leakyHcal=0, 
		int eventDisplay=0,
		TString myChain=input_chain, 
		Int_t mnEvents=0,
		std::string configFile = "sim/fast_track.xml",
		bool SiIneff = false){
  
  gSystem->Load( "libStarRoot.so" );
  gROOT->SetMacroPath(".:/star-sw/StRoot/macros/:./StRoot/macros:./StRoot/macros/graphics:./StRoot/macros/analysis:./StRoot/macros/test:./StRoot/macros/examples:./StRoot/macros/html:./StRoot/macros/qa:./StRoot/macros/calib:./StRoot/macros/mudst:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/graphics:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/analysis:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/test:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/examples:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/html:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/qa:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/calib:/afs/rhic.bnl.gov/star/packages/DEV/StRoot/macros/mudst:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/macros:/afs/rhic.bnl.gov/star/ROOT/36/5.34.38/.sl73_x8664_gcc485/rootdeb/tutorials");
  gROOT->LoadMacro("bfc.C");

  TString myDat, subDir, infile, outfile;
  TString proc(pid);
  if(proc.Contains("dy") || proc.Contains("JPsi") || proc.Contains("mb") || proc.Contains("jet") || proc.Contains("dybg")){
    subDir=Form("pythia_%s_vz%d",pid,(int)vz);
    myDat=Form("pythia_%s_vz%d_run%i.fzd",pid,(int)vz,run);    
  }else if(e>0.0){
    myDat=Form("%s.e%d.vz%d.run%i.fzd",pid,e,(int)vz,run);
  }else{
    myDat=Form("%s.pt%3.1f.vz%d.run%i.fzd",pid,pt,(int)vz,run);
  }
  if(myDir.EqualTo(".")) {infile = myDir + "/" + myDat;} 
  else                   {infile = myDir + "/" + subDir + "/" + myDat;}
  outfile = myOutDir + "/" + myDat.ReplaceAll(".fzd",".root");      
  cout << "Input  file=" << infile << endl;
  cout << "Output file=" << outfile << endl;

  bfc( -1, myChain, infile); 
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
  fcsdbmkr->setDbAccess(1);

  StFcsDb* fcsdb = (StFcsDb*) chain->GetDataSet("fcsDb");  
  cout << "fcsdb="<<fcsdb<<endl;
  //fcsdb->readGainFromText();
  //fcsdb->readGainCorrFromText();
  fcsdb->forceUniformGain(0.0053);
  fcsdb->forceUniformGainCorrection(1.0);

  StFcsFastSimulatorMaker *fcssim = (StFcsFastSimulatorMaker*) chain->GetMaker("fcsSim");
  fcssim->setDebug(debug);
  fcssim->setLeakyHcal(leakyHcal);

  StFcsWaveformFitMaker *wff=(StFcsWaveformFitMaker *)chain->GetMaker("StFcsWaveformFitMaker");
  wff->setDebug(debug);
  wff->setEnergySelect(0);

  StFcsClusterMaker *clu=(StFcsClusterMaker *)chain->GetMaker("StFcsClusterMaker");
  clu->setDebug(debug);

  /*
  StFcsPointMaker *poi=(StFcsPointMaker *)chain->GetMaker("StFcsPointMaker");
  poi->setDebug(1);
  poi->setShowerShape(3);
  */

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

  // Configure FTT FastSim
  StFttFastSimMaker *fttFastSim = (StFttFastSimMaker*) chain->GetMaker( "fttSim" );

  // Configure FST FastSim
  StFstFastSimMaker *fstFastSim = (StFstFastSimMaker*) chain->GetMaker( "fstFastSim" );;
  //TString qaoutname(gSystem->BaseName(inFile));
  //qaoutname.ReplaceAll(".fzd", ".FastSimu.QA.root");
  //fstFastSim->SetQAFileName(qaoutname);
  if (SiIneff) fstFastSim->SetInEfficiency(0.1); // inefficiency of Si   

  // Configure the Forward Tracker
  StFwdTrackMaker * fwdTrack = (StFwdTrackMaker*) chain->GetMaker( "fwdTrack" );;
  cout << "Running FwdTracking with config: " << configFile << endl;
  fwdTrack->SetConfigFile( configFile );
  //fwdTrack->SetGenerateTree( true );
  //fwdTrack->SetGenerateHistograms( true );  
  
  //FwdTrack and FcsCluster assciation
  gSystem->Load("StFcsTrackMatchMaker");
  StFcsTrackMatchMaker *match = new StFcsTrackMatchMaker();
  TString matchfile(outfile); matchfile.ReplaceAll(".root",".match.root");
  match->setFileName(matchfile.Data());
  match->SetDebug(debug);

  gSystem->Load("StFcsDiLeptonMaker"); 
  StFcsDiLeptonMaker *dilep = new StFcsDiLeptonMaker;
  TString dilepfile(outfile); dilepfile.ReplaceAll(".root",".dilep.root");  
  dilep->setFileName(dilepfile.Data());

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
