//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 31 March 2011
//

void RunJetSkim2009sim(int nevents = 1e6,
			 const char* mudstfile = "/star/data47/reco/pp200/pythia6_410/15_25gev/cdf_a/y2006c/gheisha_on/p07ic/rcf1307_01_2000evts.MuDst.root",
                         const char* geantfile = "/star/data47/reco/pp200/pythia6_410/15_25gev/cdf_a/y2006c/gheisha_on/p07ic/rcf1307_01_2000evts.geant.root",
			 const char* skimfile  = "rcf1307_01_2000evts.skim.root",
			 bool useL2 = true)
{
  cout << "Read MuDst file:\t" << mudstfile << endl;
  cout << "Read geant file:\t" << geantfile << endl;
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
  gSystem->Load("libfastjet.so");
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
  //starDb->SetDateTime(20060516,110349); // Run 7136022

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
  // -- StL2_2009EmulatorMaker must run before StTriggerSimuMaker?
  StL2_2009EmulatorMaker* simL2Mk = 0;
  if (useL2) {
    simL2Mk = new StL2_2009EmulatorMaker;
    //simL2Mk->setSetupPath("/star/u/pibero/public/StarTrigSimuSetup/");
    simL2Mk->setSetupPath("/star/institutions/mit/corliss/L2setup/");
    simL2Mk->setOutPath("./");
  }
  StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
  simuTrig->useOnlineDB(); // for trigger definitions and thresholds
  simuTrig->setMC(true); // Must be before individual detectors, to be passed
  //simuTrig->useBbc(); // No BBC in Run 9
  simuTrig->useBemc();
  simuTrig->useEemc(); // No EEMC in Run 6
  simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
  if (useL2) simuTrig->useL2(simL2Mk);

  // Get Pythia record
  StMCAsymMaker* asym = new StMCAsymMaker;

  // Skim event maker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimfile);
  skimEventMaker->addSimuTrigger(240530); // BHT3
  skimEventMaker->addSimuTrigger(240652); // L2JetHigh
  skimEventMaker->addSimuTrigger(240411); // JP1

  chain->Init();
  chain->EventLoop(nevents);
}
