//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 24 Jan 2011
//

void RunJetFinder2009sim(int nevents = 1e6,
			 const char* mudstfile = "/star/data47/reco/pp200/pythia6_410/15_25gev/cdf_a/y2006c/gheisha_on/p07ic/rcf1307_01_2000evts.MuDst.root",
                         const char* geantfile = "/star/data47/reco/pp200/pythia6_410/15_25gev/cdf_a/y2006c/gheisha_on/p07ic/rcf1307_01_2000evts.geant.root",
			 const char* jetfile   = "rcf1307_01_2000evts.jets.root",
			 const char* skimfile  = "rcf1307_01_2000evts.skim.root",
			 bool useL2 = false)
{
  cout << "Read MuDst file:\t" << mudstfile << endl;
  cout << "Read geant file:\t" << geantfile << endl;
  cout << "Write jet file:\t" << jetfile << endl;
  cout << "Write skim file:\t" << skimfile << endl;

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
  gSystem->Load("StPreEclMaker");
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
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  // Create chain
  StChain* chain = new StChain;

  // I/O maker
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(geantfile);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             // Deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   // Activate geant Branch

  // StMcEvent maker
  StMcEventMaker* mcEventMaker = new StMcEventMaker;
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;

  // MuDst reader
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",mudstfile,"",100000,"MuDst");

  // MuDst DB
  StMuDbReader* muDstDb = StMuDbReader::instance();

  // star database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");
  starDb->SetDateTime(20090628,53220); // Run 10179006

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // EEMC slow simulator
  StEEmcSlowMaker* eess = new StEEmcSlowMaker;
  eess->setSamplingFraction(0.0384);
  eess->setAddPed(true);
  eess->setSmearPed(true);

  // BEMC simulator
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker;
  emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);
  StPreEclMaker* preEcl = new StPreEclMaker; // need this to fill new StEvent information

  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;

  // Trigger simulator
  StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
  simuTrig->setMC(true); // Must be before individual detectors, to be passed
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

  // Get Pythia record
  StMCAsymMaker* asym = new StMCAsymMaker;

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);
  skimEventMaker->addSimuTrigger(240530); // BHT3
  skimEventMaker->addSimuTrigger(240652); // L2JetHigh
  skimEventMaker->addSimuTrigger(240411); // JP1

  // Jet maker
  StJetMaker2009* jetmaker = new StJetMaker2009;
  jetmaker->setJetFile(jetfile);

  //------------------------------------------------------------------------------------

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
  anapars12->addTpcCut(new StjTrackCutDcaPtDependent);
  anapars12->addTpcCut(new StjTrackCutChi2(0,4));
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

  //------------------------------------------------------------------------------------

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
  anapars5->addTpcCut(new StjTrackCutDcaPtDependent);
  anapars5->addTpcCut(new StjTrackCutChi2(0,4));
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
  anapars5->addJetCut(new StProtoJetCutEta(0.8,2.5));

  //------------------------------------------------------------------------------------

  // Set analysis cuts for EMC branch
  StAnaPars* anaparsEMC = new StAnaPars;
  anaparsEMC->useTpc  = true;
  anaparsEMC->useBemc = true;
  anaparsEMC->useEmc  = true;

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

  //------------------------------------------------------------------------------------

  // Set analysis cuts for Pythia branch
  StAnaPars* anaparsPythia = new StAnaPars;
  anaparsPythia->useMc = true;

  // Jet cuts
  anaparsPythia->addJetCut(new StProtoJetCutPt(3,200));
  anaparsPythia->addJetCut(new StProtoJetCutEta(-100,100));
    
  // Set cone jet finder parameters
  StConePars* conepars = new StConePars;
  conepars->setGridSpacing(105,-3.0,3.0,120,-TMath::Pi(),TMath::Pi());
  conepars->setSeedEtMin(0.5);
  conepars->setAssocEtMin(0.1);
  conepars->setSplitFraction(0.5);
  conepars->setPerformMinimization(true);
  conepars->setAddMidpoints(true);
  conepars->setRequireStableMidpoints(true);
  conepars->setDoSplitMerge(true);
  conepars->setDebug(false);

  //------------------------------------------------------------------------------------

  jetmaker->addBranch("ConeJets12",anapars12,conepars);
  jetmaker->addBranch("ConeJets5",anapars5,conepars);
  jetmaker->addBranch("ConeJetsEMC",anaparsEMC,conepars);
  jetmaker->addBranch("PythiaConeJets",anaparsPythia,conepars);

  //------------------------------------------------------------------------------------

  chain->Init();
  chain->EventLoop(nevents);
}
