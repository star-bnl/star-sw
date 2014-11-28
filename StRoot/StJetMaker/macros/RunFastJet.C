//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 Aug 2011
//
// $Id: RunFastJet.C,v 1.4 2012/07/16 21:50:26 pibero Exp $
//
// $Log: RunFastJet.C,v $
// Revision 1.4  2012/07/16 21:50:26  pibero
// Updated for easier reading...
//
// Revision 1.3  2012/03/10 23:10:36  pibero
// Added support for fastjet plugins
//
// Revision 1.2  2011/08/31 18:03:29  pibero
// Minor updates
//
//

void RunFastJet(int nevents = 1e6,
                const char* mudstfile = "root://xrdstar.rcf.bnl.gov:1095//home/starlib/home/starreco/reco/production2009_200Gev_Single/ReversedFullField/P10ic/2009/143/10143008/st_physics_10143008_raw_6020001.MuDst.root",
                const char* jetfile = "jets.root",
		const char* skimfile = "skim.root",
		bool useL2 = false)
{
  cout << "Read MuDst file:\t" << mudstfile << endl;
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

  // The chain
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
  simuTrig->useOnlineDB();
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

  // Set anti-kt parameters
  const double CONE_RADIUS = 0.6;

  StFastJetPars* AntiKtPars = new StFastJetPars;
  AntiKtPars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
  AntiKtPars->setRparam(CONE_RADIUS);
  AntiKtPars->setRecombinationScheme(StFastJetPars::E_scheme);
  AntiKtPars->setStrategy(StFastJetPars::Best);
  AntiKtPars->setPtMin(5.0);

  jetmaker->addBranch("ConeJets12",anapars12,AntiKtPars);
  jetmaker->addBranch("ConeJets5",anapars5,AntiKtPars);
  jetmaker->addBranch("ConeJetsEMC",anaparsEMC,AntiKtPars);

  chain->Init();
  chain->EventLoop(nevents);
}
