//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 4 Dec 2011
//

void RunJetFinder2009emb(int nevents = 1e6,
			 const char* mudstfile = "../eliza17/SL11d_embed/10120032/pt11_15_*.MuDst.root",
                         const char* geantfile = "../eliza17/SL11d_embed/10120032/pt11_15_*.geant.root",
			 const char* jetfile   = "jets.root",
			 const char* skimfile  = "skim.root",
			 bool useL2 = true)
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
  gSystem->Load("fastjet-install/lib/libfastjet.so");
  gSystem->Load("fastjet-install/lib/libfastjettools.so");
  gSystem->Load("fastjet-install/lib/libsiscone.so");
  gSystem->Load("fastjet-install/lib/libsiscone_spherical.so");
  gSystem->Load("fastjet-install/lib/libfastjetplugins.so");
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

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Barrel ADC to energy maker
  StEmcADCtoEMaker* adc2e = new StEmcADCtoEMaker;
  adc2e->saveAllStEvent(true);

  // Trigger simulator
  // -- StL2_2009EmulatorMaker must run before StTriggerSimuMaker?
  StL2_2009EmulatorMaker* simL2Mk = 0;
  if (useL2) {
    simL2Mk = new StL2_2009EmulatorMaker;
    simL2Mk->setSetupPath("/home/pibero/public/StarTrigSimuSetup/");
    simL2Mk->setOutPath("../eliza14/L2/");
  }
  StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
  simuTrig->setMC(2); // 0=data, 1=simulation, 2=embedding
  simuTrig->useBemc();
  simuTrig->useEemc();
  simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
  if (useL2) simuTrig->useL2(simL2Mk);

  // Set trigger thresholds using run 10180030
  simuTrig->setBarrelJetPatchTh(0,20);
  simuTrig->setBarrelJetPatchTh(1,28);
  simuTrig->setBarrelJetPatchTh(2,36);

  simuTrig->setOverlapJetPatchTh(0,19);
  simuTrig->setOverlapJetPatchTh(1,26);
  simuTrig->setOverlapJetPatchTh(2,34);

  simuTrig->setEndcapJetPatchTh(0,18);
  simuTrig->setEndcapJetPatchTh(1,25);
  simuTrig->setEndcapJetPatchTh(2,32);

  simuTrig->setBarrelHighTowerTh(0,11);
  simuTrig->setBarrelHighTowerTh(1,15);
  simuTrig->setBarrelHighTowerTh(2,18);
  simuTrig->setBarrelHighTowerTh(3,25);

  simuTrig->setEndcapHighTowerTh(0,16);
  simuTrig->setEndcapHighTowerTh(1,25);

  // Define triggers using run 10180030
  // triggerIndex,name,triggerId,onbits,offbits,onbits1,onbits2,onbits3,offbits1,offbits2,offbits3
  simuTrig->emc->defineTrigger(1,"BHT3",240530,0x00000001,0x2da11477,0x12c3a299,0xa6a96c01,0xc81b8346,0xc3887411,0x9011b0b1,0x3c7f7e17);
  simuTrig->emc->defineTrigger(5,"L2JetHigh",240652,0x00000002,0x8c8d4c11,0x63235304,0xd41b4de0,0xe256967e,0xb895a59f,0x299d9bfb,0x077c6927);
  simuTrig->emc->defineTrigger(18,"JP1",240411,0x00000040,0x5a488156,0xff7eacd8,0x57f9d3db,0x58073140,0x00000016,0x57ef7ff4,0x00000016);

  // Set LD301 registers
  simuTrig->setLastDsmRegister(0,1696); // BBCMBLive-PS-lo
  simuTrig->setLastDsmRegister(1,24); // BBCMBLive-PS-hi
  simuTrig->setLastDsmRegister(2,20); // VPDMBLive-PS-Lo
  simuTrig->setLastDsmRegister(3,0); // VPDMBLive-PS-hi
  simuTrig->setLastDsmRegister(4,1000); // ZDCMB-PS-lo
  simuTrig->setLastDsmRegister(5,0); // ZDCMB-PS-hi
  simuTrig->setLastDsmRegister(6,0); // BBC-Live-Det-Select
  simuTrig->setLastDsmRegister(7,0); // VPD-Live-Det-Select
  simuTrig->setLastDsmRegister(8,7); // Output-Bit1-Select
  simuTrig->setLastDsmRegister(9,11); // Output-Bit2-Select
  simuTrig->setLastDsmRegister(10,1); // JP1-Select
  simuTrig->setLastDsmRegister(11,13); // FMS-Fast-Select
  simuTrig->setLastDsmRegister(12,3); // FMS-led-FPE-Select
  simuTrig->setLastDsmRegister(13,4); // FMS-Slow-Select
  simuTrig->setLastDsmRegister(14,100); // FMS-Fast-Single-PS

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
  anapars12->randomSelectorProb = 1.00;

  const double randomAccept = 0.93;

  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  // 3. StjTowerEnergyCorrectionForTracksNull (no correction)
  anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

  // TPC cuts
  anapars12->addTpcCut(new StjTrackCutFlag(0));
  anapars12->addTpcCut(new StjTrackCutNHits(12));
  anapars12->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
  anapars12->addTpcCut(new StjTrackCutDca(3));
  anapars12->addTpcCut(new StjTrackCutTdcaPtDependent);
  anapars12->addTpcCut(new StjTrackCutPt(0.2,200));
  anapars12->addTpcCut(new StjTrackCutEta(-2.5,2.5));
  anapars12->addTpcCut(new StjTrackCutLastPoint(125));
  anapars12->addTpcCut(new StjTrackCutRandomAccept(randomAccept));

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
  anapars5->randomSelectorProb = 1.00;

  // The classes available for correcting tower energy for tracks are:
  // 1. StjTowerEnergyCorrectionForTracksMip
  // 2. StjTowerEnergyCorrectionForTracksFraction
  // 3. StjTowerEnergyCorrectionForTracksNull (no correction)
  anapars5->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));

  // TPC cuts
  anapars5->addTpcCut(new StjTrackCutFlag(0));
  anapars5->addTpcCut(new StjTrackCutNHits(5));
  anapars5->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
  anapars5->addTpcCut(new StjTrackCutDca(3));
  anapars5->addTpcCut(new StjTrackCutTdcaPtDependent);
  anapars5->addTpcCut(new StjTrackCutPt(0.2,200));
  anapars5->addTpcCut(new StjTrackCutEta(-2.5,2.5));
  anapars5->addTpcCut(new StjTrackCutRandomAccept(randomAccept));

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
  anapars5->addJetCut(new StProtoJetCutEta(-100,100));

  //------------------------------------------------------------------------------------

  // Set analysis cuts for EMC branch
  StAnaPars* anaparsEMC = new StAnaPars;
  anaparsEMC->useTpc  = true;
  anaparsEMC->useBemc = true;
  anaparsEMC->useEemc  = true;

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

  // Set analysis cuts for particle jets branch
  StAnaPars* anaparsParticle = new StAnaPars;
  anaparsParticle->useMonteCarlo = true;

  // MC cuts
  anaparsParticle->addMcCut(new StjMCParticleCutStatus(1)); // final state particles

  // Jet cuts
  anaparsParticle->addJetCut(new StProtoJetCutPt(1.5,200));
  anaparsParticle->addJetCut(new StProtoJetCutEta(-100,100));

  // Set analysis cuts for parton jets branch
  StAnaPars* anaparsParton = new StAnaPars;
  anaparsParton->useMonteCarlo = true;

  // MC cuts
  anaparsParton->addMcCut(new StjMCParticleCutParton);

  // Jet cuts
  anaparsParton->addJetCut(new StProtoJetCutPt(1.5,200));
  anaparsParton->addJetCut(new StProtoJetCutEta(-100,100));

  // Set STAR midpoint R=0.7 parameters
  StConePars* StarMidpointR070Pars = new StConePars;
  StarMidpointR070Pars->setGridSpacing(105,-3.0,3.0,120,-TMath::Pi(),TMath::Pi());
  StarMidpointR070Pars->setConeRadius(0.7);
  StarMidpointR070Pars->setSeedEtMin(0.5);
  StarMidpointR070Pars->setAssocEtMin(0.1);
  StarMidpointR070Pars->setSplitFraction(0.5);
  StarMidpointR070Pars->setPerformMinimization(true);
  StarMidpointR070Pars->setAddMidpoints(true);
  StarMidpointR070Pars->setRequireStableMidpoints(true);
  StarMidpointR070Pars->setDoSplitMerge(true);
  StarMidpointR070Pars->setDebug(false);

  // Set CDF midpoint R=0.7 parameters
  const double coneRadius = 0.7;

  StFastJetPars* CdfMidpointR070Pars = new StFastJetPars;
  CdfMidpointR070Pars->setJetAlgorithm(StFastJetPars::plugin_algorithm);
  CdfMidpointR070Pars->setRparam(coneRadius);
  CdfMidpointR070Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  CdfMidpointR070Pars->setStrategy(StFastJetPars::plugin_strategy);
  CdfMidpointR070Pars->setPtMin(1.5);

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
  AntiKtR060Pars->setPtMin(1.5);

  // Set anti-kt R=0.5 parameters
  StFastJetPars* AntiKtR050Pars = new StFastJetPars;
  AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtR050Pars->setRparam(0.5);
  AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtR050Pars->setStrategy(StFastJetPars::Best);
  AntiKtR050Pars->setPtMin(1.5);

  jetmaker->addBranch("CdfMidpointR070NHits12",anapars12,CdfMidpointR070Pars);
  jetmaker->addBranch("CdfMidpointR070NHits5",anapars5,CdfMidpointR070Pars);
  jetmaker->addBranch("CdfMidpointR070EMC",anaparsEMC,CdfMidpointR070Pars);
  jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR060NHits5",anapars5,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR060EMC",anaparsEMC,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
  jetmaker->addBranch("AntiKtR050NHits5",anapars5,AntiKtR050Pars);
  jetmaker->addBranch("AntiKtR050EMC",anaparsEMC,AntiKtR050Pars);
  jetmaker->addBranch("CdfMidpointR070Particle",anaparsParticle,CdfMidpointR070Pars);
  jetmaker->addBranch("CdfMidpointR070Parton",anaparsParton,CdfMidpointR070Pars);
  jetmaker->addBranch("AntiKtR060Particle",anaparsParticle,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR060Parton",anaparsParton,AntiKtR060Pars);
  jetmaker->addBranch("AntiKtR050Particle",anaparsParticle,AntiKtR050Pars);
  jetmaker->addBranch("AntiKtR050Parton",anaparsParton,AntiKtR050Pars);

  //------------------------------------------------------------------------------------

  chain->Init();
  chain->EventLoop(nevents);
}
