//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 10 March 2010
//

void RunJetFinder2009(int nevents = 1e6,
		      const char* mudstfile = "/star/data60/reco/production2009_200Gev_Single/ReversedFullField/P10ic/2009/143/10143008/st_physics_10143008_raw_6020001.MuDst.root",
		      const char* jetfile   = "st_physics_10143008_raw_6020001.jets.root",
		      const char* skimfile  = "st_physics_10143008_raw_6020001.skim.root",
		      bool useL2 = false)
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
  gSystem->Load("StRandomSelector");
  gSystem->Load("libfastjet.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StTriggerFilterMaker");

  StChain* chain = new StChain; 

  // MuDst reader
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,"",100000,"MuDst");

  // StMuDbReader...
  StMuDbReader* db = StMuDbReader::instance();

  // Trigger filter
  StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;

  // 2009 pp500
  filterMaker->addTrigger(230410); // JP1
  filterMaker->addTrigger(230411); // JP2
  filterMaker->addTrigger(230420); // AJP

  // 2009 pp200
  // http://www.star.bnl.gov/protected/common/common2009/trigger2009/triggers2009.html
  // BHT3
  filterMaker->addTrigger(240530);
  // L2BGamma
  filterMaker->addTrigger(240620);
  // L2EGamma
  filterMaker->addTrigger(240630);
  filterMaker->addTrigger(240631);
  // L2JetHigh
  filterMaker->addTrigger(240650);
  filterMaker->addTrigger(240651);
  filterMaker->addTrigger(240652);
  // JP1
  filterMaker->addTrigger(240410);
  filterMaker->addTrigger(240411);
  // Upsilon
  filterMaker->addTrigger(240641);
  // Luminosity monitors
  // BBCMB
  filterMaker->addTrigger(240010);
  filterMaker->addTrigger(240110);
  filterMaker->addTrigger(240120);
  filterMaker->addTrigger(240220);
  // BBCMB-Cat0
  filterMaker->addTrigger(240011);
  filterMaker->addTrigger(240111);
  filterMaker->addTrigger(240121);
  filterMaker->addTrigger(240221);
  // BBCMB-Cat1
  filterMaker->addTrigger(240012);
  filterMaker->addTrigger(240112);
  filterMaker->addTrigger(240122);
  filterMaker->addTrigger(240222);
  // BBCMB-Cat2
  filterMaker->addTrigger(240013);
  filterMaker->addTrigger(240113);
  filterMaker->addTrigger(240123);
  filterMaker->addTrigger(240223);
  // BBCMB-Cat3
  filterMaker->addTrigger(240014);
  filterMaker->addTrigger(240114);
  filterMaker->addTrigger(240124);
  filterMaker->addTrigger(240224);
  // BBCMB-Cat4
  filterMaker->addTrigger(240015);
  filterMaker->addTrigger(240115);
  filterMaker->addTrigger(240125);
  filterMaker->addTrigger(240225);
  // VPDMB
  filterMaker->addTrigger(240020);
  filterMaker->addTrigger(240025);

  // star database
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
  // BBC was not used in Run 9
  //simuTrig->useBbc();
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
  // Use StJetEvent format. Comment out the line below to use old StJets format.
  jetMaker->SetTreeWriter(new StjeJetEventTreeWriter(jetfile));

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);

  // Set the analysis cuts (See StJetMaker/StppJetAnalyzer.h -> class StppAnaPars)
  StppAnaPars* anapars = new StppAnaPars;
  anapars->setFlagMin(0); // track->flag() > 0
  anapars->setCutPtMin(0.2); // track->pt() > 0.2
  anapars->setAbsEtaMax(2.5); // abs(track->eta()) < 2.5
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
