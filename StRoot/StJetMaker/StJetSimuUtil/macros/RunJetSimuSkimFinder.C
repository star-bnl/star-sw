// NOTE - This macro is ONLY for running MC simulation data!!
//=========================================================================================

void RunJetSimuSkimFinder(const int nevents = 2000,
                          const char* file = "/star/data13/simu/pp200/pythia6_410/minbias/cdf_a/y2006c/gheisha_on/p07ic/rcf1319_01_1688evts.MuDst.root",
                          const char* fname = "/star/data13/simu/pp200/pythia6_410/minbias/cdf_a/y2006c/gheisha_on/p07ic/rcf1319_01_1688evts.geant.root",
			  const char* outfile = "Jets_minbias_01.root",
			  const char* skimFile = "Skim_minbias_01.root")
{
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
  gSystem->Load("StPreEclMaker");
  gSystem->Load("StEpcMaker");
  gSystem->Load("StEmcSimulatorMaker");
  gSystem->Load("StEmcUtil");
  gSystem->Load("StDbBroker");
  gSystem->Load("St_db_Maker");
  gSystem->Load("StEEmcUtil");
  gSystem->Load("StEEmcDbMaker");
  gSystem->Load("StSpinDbMaker");
  gSystem->Load("StEmcTriggerMaker");
  gSystem->Load("StTriggerUtilities");
  gSystem->Load("StMCAsymMaker");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetMaker");

  cout << " loading done " << endl;
  
  StChain* chain = new StChain; 
  chain->SetDebug(1);
  
  StIOMaker* ioMaker = new StIOMaker;
  ioMaker->SetFile(fname);
  ioMaker->SetIOMode("r");
  ioMaker->SetBranch("*",0,"0");             //deactivate all branches
  ioMaker->SetBranch("geantBranch",0,"r");   //activate geant Branch
  
  StMcEventMaker *mcEventMaker = new StMcEventMaker;
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;
  
  //Instantiate the MuDstReader
  StMuDebug::setLevel(1); 
  StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,"",file,"",1e6,"MuDst");
  
  //Database -- get a real calibration (this is ok, MLM)
  St_db_Maker* dbMk = new St_db_Maker("StarDb","MySQL:StarDb","MySQL:StarDb","$STAR/StarDb");
  //dbMk->SetDateTime(20050506,214129); // for simulation
  dbMk->SetDateTime(20060516,110349); // Run 7136022
   
  //Database interface
  StDetectorDbMaker* detDbMk = new StDetectorDbMaker;
  
  //Endcap DB
  StEEmcDbMaker* eemcb = new StEEmcDbMaker("eemcDb");
  
  //get BEMC calibration
  StEmcSimulatorMaker* emcSim = new StEmcSimulatorMaker; //use this instead to "redo" converstion from geant->adc
  emcSim->setCalibSpread(kBarrelEmcTowerId,0.15);
  StPreEclMaker* preEcl = new StPreEclMaker; //need this to fill new StEvent information
  
  //get trigger
  StEmcTriggerMaker* emcTrig = new StEmcTriggerMaker("bemctrigger");

  //get pythia record
  StMCAsymMaker* asym = new StMCAsymMaker("MCAsym");

  //get skimMaker
  StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker",muDstMaker,skimFile);
  //skimEventMaker->addSimuTrigger(127501);
  //skimEventMaker->addSimuTrigger(137501);
  //skimEventMaker->addSimuTrigger(137213);  
  //skimEventMaker->addSimuTrigger(127221);
  skimEventMaker->addSimuTrigger(137222); // bemc-jp1-mb, th1=60(8.3 GeV)

  //test Mike's new 4p maker:
  bool doTowerSwapFix = true;
  StBET4pMaker* bet4pMaker = new StBET4pMaker("BET4pMaker",muDstMaker, doTowerSwapFix);
  
  //Pythia4pMaker
  StPythiaFourPMaker* pythiaFourPMaker = new StPythiaFourPMaker;
  
  //Instantiate the JetMaker
  StJetMaker* emcJetMaker = new StJetMaker("emcJetMaker",muDstMaker,outfile);
  
  StppAnaPars* anapars = new StppAnaPars();
  anapars->setFlagMin(0); //track->flag() > 0
  anapars->setNhits(20); //track->nHitsFit()>20
  anapars->setCutPtMin(0.2); //track->pt() > 0.2
  anapars->setAbsEtaMax(1.6); //abs(track->eta())<1.6
  anapars->setJetPtMin(3.0);
  anapars->setJetEtaMax(100.0);
  anapars->setJetEtaMin(0);
  anapars->setJetNmin(0);

  StConePars* cpars4 = new StConePars;
  cpars4->setGridSpacing(56, -1.6, 1.6, 120, -TMath::Pi(), TMath::Pi());
  cpars4->setConeRadius(0.4);
  cpars4->setSeedEtMin(0.5);
  cpars4->setAssocEtMin(0.1);
  cpars4->setSplitFraction(0.5);
  cpars4->setPerformMinimization(true);
  cpars4->setAddMidpoints(true);
  cpars4->setRequireStableMidpoints(true);
  cpars4->setDoSplitMerge(true);
  cpars4->setDebug(false);
  emcJetMaker->addAnalyzer(anapars,cpars4,bet4pMaker,"MkConeR04");  

  StppAnaPars* pythiapars = new StppAnaPars;
  pythiapars->setFlagMin(0);
  pythiapars->setNhits(0);
  pythiapars->setCutPtMin(0.0001);
  pythiapars->setAbsEtaMax(5.0);
  pythiapars->setJetPtMin(3.0);
  pythiapars->setJetEtaMax(5.0);
  pythiapars->setJetEtaMin(0);
  pythiapars->setJetNmin(0);

  StConePars* pythia_cpars4 = new StConePars;
  pythia_cpars4->setGridSpacing(200, -5.0, 5.0, 120, -TMath::Pi(), TMath::Pi());
  pythia_cpars4->setConeRadius(0.4);
  pythia_cpars4->setSeedEtMin(0.5);
  pythia_cpars4->setAssocEtMin(0.1);
  pythia_cpars4->setSplitFraction(0.5);
  pythia_cpars4->setPerformMinimization(true);
  pythia_cpars4->setAddMidpoints(true);
  pythia_cpars4->setRequireStableMidpoints(true);
  pythia_cpars4->setDoSplitMerge(true);
  pythia_cpars4->setDebug(false);
  emcJetMaker->addAnalyzer(pythiapars, pythia_cpars4, pythiaFourPMaker, "PythiaConeR04");
 
  chain->Init();
  chain->PrintInfo();
  chain->ls(3);
  chain->EventLoop(nevents);
}
