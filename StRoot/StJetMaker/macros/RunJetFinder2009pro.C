//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

void RunJetFinder2009pro(int nevents = 1e7,
			 const char* mudstfile = "/star/data65/reco/production2009_200Gev_Single/FullField/P11id/2009/150/10150053/st_physics_10150053_raw_4030003.MuDst.root",
			 const char* jetfile = "jets.root",
			 const char* skimfile = "skim.root",
			 bool useL2 = false)
{
  cout << "mudstfile = " << mudstfile << endl;
  cout << "jetfile = " << jetfile << endl;
  cout << "skimfile = " << skimfile << endl;

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
  gSystem->Load("libCDFConesPlugin.so");
  gSystem->Load("libEECambridgePlugin.so");
  gSystem->Load("libJadePlugin.so");
  gSystem->Load("libNestedDefsPlugin.so");
  gSystem->Load("libSISConePlugin.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
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

  // 2009 pp200
  // http://www.star.bnl.gov/protected/common/common2009/trigger2009/triggers2009.html
  // L2JetHigh
  filterMaker->addTrigger(240650);
  filterMaker->addTrigger(240651);
  filterMaker->addTrigger(240652);
  // JP1
  filterMaker->addTrigger(240410);
  filterMaker->addTrigger(240411);
  // L2BGamma
  filterMaker->addTrigger(240620);
  // L2EGamma
  filterMaker->addTrigger(240630);
  filterMaker->addTrigger(240631);
  // BHT3
  filterMaker->addTrigger(240530);
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

  // 2011 pp500 transverse
  filterMaker->addTrigger(320600); // JP0
  filterMaker->addTrigger(320601); // JP1
  filterMaker->addTrigger(320602); // JP2*L2JetHigh
  filterMaker->addTrigger(320603); // AJP
  filterMaker->addTrigger(320800); // BHT2*JP1*L2Bgamma
  filterMaker->addTrigger(320801); // BHT3*L2BW
  filterMaker->addTrigger(320850); // EHT0*JP1*L2Egamma
  filterMaker->addTrigger(320851); // EHT1*L2EW

  // 2011 pp500 longitudinal
  filterMaker->addTrigger(330600); // JP0
  filterMaker->addTrigger(330601); // JP1
  filterMaker->addTrigger(330603); // AJP
  filterMaker->addTrigger(330612); // JP2*L2JetHigh
  filterMaker->addTrigger(330800); // BHT2*JP1*L2Bgamma
  filterMaker->addTrigger(330801); // BHT3*L2BW
  filterMaker->addTrigger(330850); // EHT0*JP1*L2Egamma
  filterMaker->addTrigger(330851); // EHT1*L2EW

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

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);

  // Jet maker
  StJetMaker2009* jetmaker = new StJetMaker2009;
  jetmaker->setJetFile(jetfile);

  // Set analysis cuts for 12-point branch
  StAnaPars* anapars12 = new StAnaPars;
  anapars12->useTpc  = true;
  anapars12->useBemc = true;
  anapars12->useEemc = true;

  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  // 3. StjTowerEnergyCorrectionForTracksNull (default: no correction)
  anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

  // TPC cuts
  anapars12->addTpcCut(new StjTrackCutFlag(0));
  anapars12->addTpcCut(new StjTrackCutNHits(12));
  anapars12->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
  anapars12->addTpcCut(new StjTrackCutDca(3));
  //anapars12->addTpcCut(new StjTrackCutDcaPtDependent);
  anapars12->addTpcCut(new StjTrackCutTdcaPtDependent);
  //anapars12->addTpcCut(new StjTrackCutChi2(0,4));
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

  // Set analysis cuts for 5-point branch
  StAnaPars* anapars5 = new StAnaPars;
  anapars5->useTpc  = true;
  anapars5->useBemc = true;
  anapars5->useEemc = true;

  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  // 3. StjTowerEnergyCorrectionForTracksNull (default: no correction)
  anapars5->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

  // TPC cuts
  anapars5->addTpcCut(new StjTrackCutFlag(0));
  anapars5->addTpcCut(new StjTrackCutNHits(5));
  anapars5->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
  anapars5->addTpcCut(new StjTrackCutDca(3));
  //anapars5->addTpcCut(new StjTrackCutDcaPtDependent);
  anapars5->addTpcCut(new StjTrackCutTdcaPtDependent);
  //anapars5->addTpcCut(new StjTrackCutChi2(0,4));
  anapars5->addTpcCut(new StjTrackCutPt(0.2,200));
  anapars5->addTpcCut(new StjTrackCutEta(-2.5,2.5));

  // BEMC cuts
  anapars5->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
  anapars5->addBemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
  anapars5->addBemcCut(new StjTowerEnergyCutEt(0.2));

  // EEMC cuts
  anapars5->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
  anapars5->addEemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
  anapars5->addEemcCut(new StjTowerEnergyCutEt(0.2));

  // Jet cuts
  anapars5->addJetCut(new StProtoJetCutPt(5,200));
  //anapars5->addJetCut(new StProtoJetCutEta(0.8,2.5));
  anapars5->addJetCut(new StProtoJetCutEta(-100,100));

  // Set analysis cuts for EMC branch
  StAnaPars* anaparsEMC = new StAnaPars;
  anaparsEMC->useTpc  = true;
  anaparsEMC->useBemc = true;
  anaparsEMC->useEemc = true;

  // TPC cuts
  anaparsEMC->addTpcCut(new StjTrackCutFlag(0));
  anaparsEMC->addTpcCut(new StjTrackCutNHits(1000000));

  // BEMC cuts
  anaparsEMC->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
  anaparsEMC->addBemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
  anaparsEMC->addBemcCut(new StjTowerEnergyCutEt(0.2));

  // EEMC cuts
  anaparsEMC->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
  anaparsEMC->addEemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
  anaparsEMC->addEemcCut(new StjTowerEnergyCutEt(0.2));

  // Jet cuts
  anaparsEMC->addJetCut(new StProtoJetCutPt(5,200));
  anaparsEMC->addJetCut(new StProtoJetCutEta(-100,100));

  // Set cone jet finder parameters
  StConePars* conepars = new StConePars;
  conepars->setGridSpacing(105,-3.0,3.0,120,-TMath::Pi(),TMath::Pi());
  conepars->setConeRadius(0.7);
  conepars->setSeedEtMin(0.5);
  conepars->setAssocEtMin(0.1);
  conepars->setSplitFraction(0.5);
  conepars->setPerformMinimization(true);
  conepars->setAddMidpoints(true);
  conepars->setRequireStableMidpoints(true);
  conepars->setDoSplitMerge(true);
  conepars->setDebug(false);

  // Set CDF midpoint R=0.7 parameters
  const double coneRadius = 0.7;
  StFastJetPars* CdfMidpointR070Pars = new StFastJetPars;
  CdfMidpointR070Pars->setJetAlgorithm(StFastJetPars::plugin_algorithm);
  CdfMidpointR070Pars->setRparam(coneRadius);
  CdfMidpointR070Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  CdfMidpointR070Pars->setStrategy(StFastJetPars::plugin_strategy);
  CdfMidpointR070Pars->setPtMin(5.0);

  const double overlapThreshold = 0.75;
  const double seedThreshold = 0.5;
  const double coneAreaFraction = 1.0;

  StPlugin* cdf = new StCDFMidPointPlugin(coneRadius,overlapThreshold,seedThreshold,coneAreaFraction);
  CdfMidpointR070Pars->setPlugin(cdf);

  // Set anti-kt R=0.6 parameters
  StFastJetPars* AntiKtR060Pars = new StFastJetPars;
  AntiKtR060Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR060Pars->setRparam(0.6);
  AntiKtR060Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR060Pars->setStrategy(StFastJetPars::Best);
  AntiKtR060Pars->setPtMin(5.0);

  // Set anti-kt R=0.5 parameters
  StFastJetPars* AntiKtR050Pars = new StFastJetPars;
  AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR050Pars->setRparam(0.5);
  AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR050Pars->setStrategy(StFastJetPars::Best);
  AntiKtR050Pars->setPtMin(5.0);

  jetmaker->addBranch("CdfMidpointR070NHits12",anapars12,CdfMidpointR070Pars);
  jetmaker->addBranch("CdfMidpointR070NHits5",anapars5,CdfMidpointR070Pars);
  jetmaker->addBranch("CdfMidpointR070EMC",anaparsEMC,CdfMidpointR070Pars);
  jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR060NHits5",anapars5,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR060EMC",anaparsEMC,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
  jetmaker->addBranch("AntiKtR050NHits5",anapars5,AntiKtR050Pars);
  jetmaker->addBranch("AntiKtR050EMC",anaparsEMC,AntiKtR050Pars);

  // Run
  chain->Init();
  chain->EventLoop(nevents);
}
