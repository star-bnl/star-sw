void RunJetFinder2(int nevents = 100,
		   const char* mudstfile = "/star/data54/reco/ppProductionTrans/FullField/P06ie/2006/117/7117002/st_physics_7117002_raw_1040035.MuDst.root",
                   const char* jetfile = "blah.jet.root",
                   const char* skimfile = "blah.skim.root")
{
  cout << "Read MuDst file:\t" << mudstfile << endl;
  cout << "Write jet file:\t" << jetfile << endl;
  cout << "Write skim file:\t" << skimfile << endl;

  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");

  gSystem->Load("StTpcDb");
  gSystem->Load("StDetectorDbMaker");
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
  gSystem->Load("libfastjet.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetMaker");

  StChain* chain = new StChain; 

  // MuDst reader
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,"",100000,"MuDst");

  //StMuDbReader...
  StMuDbReader* db = StMuDbReader::instance();

  //StMuDst2StEventMaker
  //StMuDst2StEventMaker* eventMaker = new StMuDst2StEventMaker("MuDst2StEvent");

  // STAR database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Spin database
  StSpinDbMaker* spinDb = new StSpinDbMaker;

  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;

  // Trigger simulator
  StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
  simuTrig->setMC(false); // Must be before individual detectors, to be passed
  simuTrig->useBbc();
  simuTrig->useBemc();
  simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
  StGenericL2Emulator* simL2Mk = new StL2_2006EmulatorMaker;
  assert(simL2Mk);
  simL2Mk->setSetupPath("/afs/rhic.bnl.gov/star/users/kocolosk/public/StarTrigSimuSetup/");
  simL2Mk->setOutPath("./");
  simuTrig->useL2(simL2Mk);

  // Add Mike's 4p maker.
  // Here we also tag whether or not to do the swap.
  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  bool doTowerSwapFix = true;
  StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker,doTowerSwapFix,new StjTowerEnergyCorrectionForTracksFraction(1.00));
  bet4pMaker->setUseTPC(true);
  bet4pMaker->setUseBEMC(true);
  bet4pMaker->setUseEndcap(true);
  bet4pMaker->setUse2003Cuts(false);
  bet4pMaker->setUse2005Cuts(false);
  bet4pMaker->setUse2006Cuts(true);

  // Jet maker
  StJetMaker* jetMaker = new StJetMaker("jetMaker",muDstMaker,jetfile);

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);

  //Instantiate Jet Histogram Maker
  //StJetHistMaker* jetHistMaker = new StJetHistMaker(muDstMaker, histfile.Data() );

  // Set the analysis cuts (See StJetMaker/StppJetAnalyzer.h -> class StppAnaPars)
  StppAnaPars* anapars = new StppAnaPars;
  anapars->setFlagMin(0); // track->flag() > 0
  anapars->setCutPtMin(0.2); // track->pt() > 0.2
  anapars->setAbsEtaMax(1.6); // abs(track->eta()) < 1.6
  anapars->setJetPtMin(5.0);
  anapars->setJetEtaMax(100.0);
  anapars->setJetEtaMin(0);
  anapars->setJetNmin(0);

  // Setup the cone finder (See StJetFinder/StConeJetFinder.h -> class StConePars)
  StConePars* cpars = new StConePars;
  cpars->setGridSpacing(105,-3.0,3.0,120,-TMath::Pi(),TMath::Pi());
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);

  jetMaker->addAnalyzer(anapars,cpars,bet4pMaker,"ConeJets12");

  anapars->setNhits(5);
  jetMaker->addAnalyzer(anapars,cpars,bet4pMaker,"ConeJets5");

  anapars->setNhits(1000000);
  jetMaker->addAnalyzer(anapars,cpars,bet4pMaker,"ConeJetsEMC");

  chain->Init();
  chain->EventLoop(nevents);
}
