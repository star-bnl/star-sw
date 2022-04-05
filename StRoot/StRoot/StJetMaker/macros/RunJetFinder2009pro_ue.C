//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Laboratory
// 10 August 2015
// Added UE to the JetFinder
//

void RunJetFinder2009pro_ue(int nevents = 1E3,
			 const char* mudstfile = "/star/institutions/uky/gdwebb/UE_histos/st_physics_10103041_raw_8030001.MuDst.root",
			 const char* jetfile = "jets_copy.root",
			 const char* skimfile = "skim_copy.root",
			 const char* uefile = "ueTree_copy.root",
			 int mEmbed = 0,
			 int mPythia = 0,
			 bool useL2 = false)
{
  cout << "nevents = " << nevents << endl;
  cout << "mudstfile = " << mudstfile << endl;
  cout << "jetfile = " << jetfile << endl;
  cout << "skimfile = " << skimfile << endl;
  cout << "uefile = " << uefile << endl;

  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");

  gSystem->Load("StDetectorDbMaker");
  gSystem->Load("StTpcDb");
  gSystem->Load("StDbUtilities");
  gSystem->Load("StMcEvent");
  gSystem->Load("StMcEventMaker");
  gSystem->Load("StDaqLib");
  gSystem->Load("StEmcRawMaker");
  gSystem->Load("StEmcADCtoEMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StRandomSelector");

  gSystem->Load("libfastjet.so");
  gSystem->Load("libsiscone.so");
  gSystem->Load("libsiscone_spherical.so");
  gSystem->Load("libfastjetplugins.so");

  // gSystem->Load("libfastjet.so");
  // gSystem->Load("libCDFConesPlugin.so");
  // gSystem->Load("libEECambridgePlugin.so");
  // gSystem->Load("libJadePlugin.so");
  // gSystem->Load("libNestedDefsPlugin.so");
  // gSystem->Load("libSISConePlugin.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StUeEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StTriggerFilterMaker");

  StChain* chain = new StChain; 

  // MuDst reader
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,"",100000,"MuDst");

  // MuDst DB
  StMuDbReader* muDstDb = StMuDbReader::instance();

  // Trigger filter
  StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;

  // 2009 pp500
  filterMaker->addTrigger(230410); // JP1
  filterMaker->addTrigger(230411); // JP2
  filterMaker->addTrigger(230420); // AJP
  filterMaker->addTrigger(230531); // BHT3

  // star database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Spin database
  StSpinDbMaker* spinDb = new StSpinDbMaker;

  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
  adc->saveAllStEvent(true);

  // Trigger simulator
  StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
  simuTrig->useOnlineDB(); // for trigger definitions and thresholds
  simuTrig->setMC(false); // Must be before individual detectors, to be passed

  simuTrig->useBemc();
  simuTrig->useEemc();
  simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);

  // L2 (only L2btowCalib, L2etowCalib, L2ped, L2jet in CVS as of 17 April 2010)
  if (useL2) {
    StL2_2009EmulatorMaker* simL2Mk = new StL2_2009EmulatorMaker;
    assert(simL2Mk);
    simL2Mk->setSetupPath("/star/u/pibero/public/StarTrigSimuSetup/");
    simL2Mk->setOutPath("./");
    simuTrig->useL2(simL2Mk);
  }

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);

  // Jet maker
  StJetMaker2009* jetmaker = new StJetMaker2009;
  jetmaker->setJetFile(jetfile);
  // UE maker
  StUEMaker2009* uemaker = new StUEMaker2009;
  uemaker->setUeFile(uefile);
  
  // Set analysis cuts for 12-point branch
  StAnaPars* anapars12 = new StAnaPars;
  anapars12->useTpc  = true;
  anapars12->useBemc = true;
  anapars12->useEemc = false;

  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  // 3. StjTowerEnergyCorrectionForTracksNull (default: no correction)
  anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));
 
  //
  // 1. StjTrackPtFraction
  // 2. StjTowerEnergyFraction
  //    The input parameter is the fraction which you want to increase or decrease the track/tower pT/energy. 
  //    i.e. If you want to decrease the track pT by 5%, you should put in -0.05 as the input parameter for StjTrackPtFraction
  //  anapars12->setTrackShift(new StjTrackPtFraction(-0.05));
  // anapars12->setTowerShift(new StjTowerEnergyFraction(-0.05));
  
  // TPC cuts
  anapars12->addTpcCut(new StjTrackCutFlag(0));
  anapars12->addTpcCut(new StjTrackCutNHits(12));
  anapars12->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
  anapars12->addTpcCut(new StjTrackCutDca(3));
  anapars12->addTpcCut(new StjTrackCutTdcaPtDependent);
  anapars12->addTpcCut(new StjTrackCutPt(0.2,200));
  anapars12->addTpcCut(new StjTrackCutEta(-2.5,2.5));
  anapars12->addTpcCut(new StjTrackCutLastPoint(125));

  // BEMC cuts
  anapars12->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
  anapars12->addBemcCut(new StjTowerEnergyCutAdc(4,3));	// ADC-ped>4 AND ADC-ped>3*RMS
  anapars12->addBemcCut(new StjTowerEnergyCutEt(0.2));

  // EEMC cuts
  anapars12->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
  anapars12->addEemcCut(new StjTowerEnergyCutAdc(4,3));	// ADC-ped>4 AND ADC-ped>3*RMS
  anapars12->addEemcCut(new StjTowerEnergyCutEt(0.2));

  // Jet cuts
  anapars12->addJetCut(new StProtoJetCutPt(5,200));
  anapars12->addJetCut(new StProtoJetCutEta(-100,100));
 
  //---------Region Criteria---------
  anapars12_toward = anapars12; // Toward Region for Tracks and Towers
  anapars12_toward->setTrackRegion(new StjTrackRegion(60.0,-60.0,1.0));
  anapars12_toward->setTowerRegion(new StjTowerRegion(60.0,-60.0,1.0));

  anapars12_away = anapars12; // Away Region for Tracks and Towers
  anapars12_away->setTrackRegion(new StjTrackRegion(120.0,-120.0,1.0));
  anapars12_away->setTowerRegion(new StjTowerRegion(120.0,-120.0,1.0));

  anapars12_transPlus = anapars12; // Trans Plus for Tracks and Towers
  anapars12_transPlus->setTrackRegion(new StjTrackRegion(120.0,60.0,1.0));
  anapars12_transPlus->setTowerRegion(new StjTowerRegion(120.0,60.0,1.0));

  anapars12_transMinus = anapars12; // Trans Minus for Tracks and Towers
  anapars12_transMinus->setTrackRegion(new StjTrackRegion(-60.0,-120.0,1.0));
  anapars12_transMinus->setTowerRegion(new StjTowerRegion(-60.0,-120.0,1.0));
  //---------------------------------

  //Jet Area 
  StFastJetAreaPars *JetAreaPars = new StFastJetAreaPars;
  JetAreaPars->setGhostArea(0.04);
  // Set anti-kt R=0.6 parameters
  StFastJetPars* AntiKtR060Pars = new StFastJetPars;
  AntiKtR060Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR060Pars->setRparam(0.6);
  AntiKtR060Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR060Pars->setStrategy(StFastJetPars::Best);
  AntiKtR060Pars->setPtMin(5.0);
  AntiKtR060Pars->setJetArea(JetAreaPars);

  // Set anti-kt R=0.4 parameters
  StFastJetPars* AntiKtR040Pars = new StFastJetPars;
  AntiKtR040Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR040Pars->setRparam(0.4);
  AntiKtR040Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR040Pars->setStrategy(StFastJetPars::Best);
  AntiKtR040Pars->setPtMin(5.0);
  AntiKtR040Pars->setJetArea(JetAreaPars);

  // Set anti-kt R=0.5 parameters
  StFastJetPars* AntiKtR050Pars = new StFastJetPars;
  AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR050Pars->setRparam(0.5);
  AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR050Pars->setStrategy(StFastJetPars::Best);
  AntiKtR050Pars->setPtMin(5.0);
  AntiKtR050Pars->setJetArea(JetAreaPars);

  jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR040NHits12",anapars12,AntiKtR040Pars);
  jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars); 

  uemaker->addBranch("toward",anapars12_toward,"AntiKtR060NHits12");
  uemaker->addBranch("away",anapars12_away,"AntiKtR060NHits12");
  uemaker->addBranch("transP",anapars12_transPlus,"AntiKtR060NHits12");
  uemaker->addBranch("transM",anapars12_transMinus,"AntiKtR060NHits12");


  // Run
  chain->Init();
  chain->EventLoop(nevents);
}
  
