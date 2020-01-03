void RunJetFinder2012pro(int nevents = 1000000,
const char* indir = "~/2013-08-trgsimu/MuDst/",
const char* MuDst = "st_physics_13109014_raw_1020001.MuDst.root",
const char* Jetfile = "st_physics_13109014_raw_1020001.jets.root",
const char* Uefile = "st_physics_13109014_raw_1020001.ueoc.root",
const char* Skimfile = "st_physics_13109014_raw_1020001.skim.root"
)
{
  cout<<"MuDst file is "<<MuDst<<endl;
  cout<<"JetTree file is "<<Jetfile<<endl;
  cout<<"SkimTree file is "<<Skimfile<<endl;

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
 
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StUeEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StTriggerFilterMaker");

  //
  gSystem->Load("libMinuit.so");
  gSystem->Load("StFmsUtil");
  gSystem->Load("StFmsDbMaker");
  gSystem->Load("StFmsHitMaker");
  gSystem->Load("StFmsPointMaker");
  gSystem->Load("StFmsQAHistoMaker");
  
   StChain *chain = new StChain;

   StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,indir, MuDst,"",1000);

   StMuDbReader* muDstDb = StMuDbReader::instance();

   StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
   //JP0
   filterMaker->addTrigger(380401);
   //JP1
   filterMaker->addTrigger(380402);
   //JP2
   filterMaker->addTrigger(380403);
   //AJP
   filterMaker->addTrigger(380404);
   
   St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

   StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

   StSpinDbMaker* spinDb = new StSpinDbMaker;

   StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
   adc->saveAllStEvent(true);

   StFmsDbMaker* fmsDb = new StFmsDbMaker;
   StFmsHitMaker* fmshitMk = new StFmsHitMaker();
   StFmsPointMaker* fmsptMk = new StFmsPointMaker("StFmsPointMaker");

   StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
   simuTrig->useOnlineDB();
   simuTrig->setMC(false);

   //simuTrig->useBbc();
   simuTrig->useBemc();
   simuTrig->useEemc();
   simuTrig->bemc->setConfig(StBemcTriggerSimu::kOnline);

   StJetMaker2012* jetmaker = new StJetMaker2012;
   jetmaker->setJetFile(Jetfile);
   jetmaker->setJetFileUe(Uefile);

   StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker", muDstMaker, Skimfile);

   StAnaPars* anapars12 = new StAnaPars;
   anapars12->useTpc = true;
   anapars12->useBemc = true;
   anapars12->useEemc = true;
   anapars12->useFms = false;

   // The classes available for correcting tower energy for tracks are:
   // 1. StjTowerEnergyCorrectionForTracksMip
   // 2. StjTowerEnergyCorrectionForTracksFraction
   // 3. StjTowerEnergyCorrectionForTracksNull (default: no correction)
   anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));
   anapars12->addTpcCut(new StjTrackCutFlag(0));
   anapars12->addTpcCut(new StjTrackCutNHits(12));
   anapars12->addTpcCut(new StjTrackCutPossibleHitRatio(0.51));
   anapars12->addTpcCut(new StjTrackCutDca(3));
   //DcaD pT dependent cut for pp200 run9
   //   anapars12->addTpcCut(new StjTrackCutDcaPtDependent);
   //DcaT pT dependent cut for pp500 run11, run12
   anapars12->addTpcCut(new StjTrackCutTdcaPtDependent);
   //Don't Need Chi2 cut for Run12 either 
   //anapars12->addTpcCut(new StjTrackCutChi2(0,4));
   anapars12->addTpcCut(new StjTrackCutPt(0.2,200));
   anapars12->addTpcCut(new StjTrackCutEta(-2.5,2.5));
   anapars12->addTpcCut(new StjTrackCutLastPoint(125));

   // BEMC cuts
   anapars12->addBemcCut(new StjTowerEnergyCutBemcStatus(1));
   anapars12->addBemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
   anapars12->addBemcCut(new StjTowerEnergyCutEt(0.2));

   // EEMC cuts
   anapars12->addEemcCut(new StjTowerEnergyCutBemcStatus(1));
   anapars12->addEemcCut(new StjTowerEnergyCutAdc(4,3)); // ADC-ped>4 AND ADC-ped>3*RMS
   anapars12->addEemcCut(new StjTowerEnergyCutEt(0.2));

   // Jet cuts
   anapars12->addJetCut(new StProtoJetCutPt(5,200));
   anapars12->addJetCut(new StProtoJetCutEta(-100,100));
   // Jet Area
   StFastJetAreaPars *JetAreaPars = new StFastJetAreaPars;
   //Anti-kT R=0.6 for run12 jet finding
   // Set anti-kt R=0.6 parameters
   StFastJetPars* AntiKtR060Pars = new StFastJetPars;
   
   AntiKtR060Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
   AntiKtR060Pars->setRparam(0.6);
   AntiKtR060Pars->setRecombinationScheme(StFastJetPars::E_scheme);
   AntiKtR060Pars->setStrategy(StFastJetPars::Best);
   AntiKtR060Pars->setPtMin(5.0);
   AntiKtR060Pars->setJetArea(JetAreaPars);
   //Anti-kT R=0.5 for run12 jet finding
   // Set anti-kt R=0.5 parameters
   StFastJetPars* AntiKtR050Pars = new StFastJetPars;
   AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
   AntiKtR050Pars->setRparam(0.5);
   AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
   AntiKtR050Pars->setStrategy(StFastJetPars::Best);
   AntiKtR050Pars->setPtMin(5.0);
   AntiKtR050Pars->setJetArea(JetAreaPars);

   jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
   jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
   StOffAxisConesPars *off050 = new StOffAxisConesPars(0.5);
   StOffAxisConesPars *off060 = new StOffAxisConesPars(0.6);
   jetmaker->addUeBranch("OffAxisConesR050", off050);
   jetmaker->addUeBranch("OffAxisConesR060", off060); 
   // Run
   chain->Init();
   chain->EventLoop(nevents);
}
