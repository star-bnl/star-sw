void RunJetFinder2012pro(int nevents = 1000000,
const char* indir = "~zchang/2013-08-trgsimu/MuDst/",
const char* MuDst = "st_physics_13109014_raw_1020001.MuDst.root",
//const char* outdir ="~/data05/Run12Jets/",
const char* Jetfile = "st_physics_13109014_raw_1020001.jets.root",
const char* Uefile = "st_physics_13109014_raw_1020001.ueoc.root",
const char* Skimfile = "st_physics_13109014_raw_1020001.skim.root"
//, bool useL2 = false
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
//  gSystem->Load("libCDFConesPlugin.so");
//  gSystem->Load("libEECambridgePlugin.so");
//  gSystem->Load("libJadePlugin.so");
//  gSystem->Load("libNestedDefsPlugin.so");
//  gSystem->Load("libSISConePlugin.so");
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
   
   //JP2*L2JetHigh
   //filterMaker->addTrigger(380405);
   //BHT2*BJP1
   //filterMaker->addTrigger(380205);
   //BHT2*BJP1*L2Bgamma
   //filterMaker->addTrigger(380208);
   //EHT0*EJP1*L2Egamma
   //filterMaker->addTrigger(380304);
   //BHT3*L2BW
   //filterMaker->addTrigger(380209);
   //filterMaker->addTrigger(380219);
   //EHT1*L2EW
   //filterMaker->addTrigger(380305);
   
   St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");

   StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

   StSpinDbMaker* spinDb = new StSpinDbMaker;

   StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
   adc->saveAllStEvent(true);

   StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
   simuTrig->useOnlineDB();
   simuTrig->setMC(false);
   simuTrig->askTrigger("JP0");
   simuTrig->askTrigger("JP1");
   simuTrig->askTrigger("JP2");
   simuTrig->askTrigger("AJP");
//   simuTrig->emc->defineTrigger2012(13,"JP0",380401,0x8000);
//   simuTrig->emc->defineTrigger2012(14,"JP1",380402,0x0040);
//   simuTrig->emc->defineTrigger2012(15,"JP2",380403,0x0080);
//   simuTrig->emc->defineTrigger2012(16,"AJP",380404,0x1000);

   simuTrig->useBbc();
   simuTrig->useBemc();
   simuTrig->useEemc();
   simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);

   StJetMaker2012* jetmaker = new StJetMaker2012;
   jetmaker->setJetFile(Jetfile);
   jetmaker->setJetFileUe(Uefile);

   StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker", muDstMaker, Skimfile);

   StAnaPars* anapars12 = new StAnaPars;
   anapars12->useTpc = true;
   anapars12->useBemc = true;
   anapars12->useEemc = true;

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

   //  jetmaker->addBranch("CdfMidpointR070NHits12",anapars12,CdfMidpointR070Pars);
   //  jetmaker->addBranch("CdfMidpointR070NHits5",anapars5,CdfMidpointR070Pars);
   //  jetmaker->addBranch("CdfMidpointR070EMC",anaparsEMC,CdfMidpointR070Pars);
   jetmaker->addBranch("AntiKtR060NHits12",anapars12,AntiKtR060Pars);
   //  jetmaker->addBranch("AntiKtR060NHits5",anapars5,AntiKtR060Pars);
   //  jetmaker->addBranch("AntiKtR060EMC",anaparsEMC,AntiKtR060Pars);
   jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
   //  jetmaker->addBranch("AntiKtR050NHits5",anapars5,AntiKtR050Pars);
   //  jetmaker->addBranch("AntiKtR050EMC",anaparsEMC,AntiKtR050Pars);
   StOffAxisConesPars *off050 = new StOffAxisConesPars(0.5);
   StOffAxisConesPars *off060 = new StOffAxisConesPars(0.6);
   jetmaker->addUeBranch("OffAxisConesR050", off050);
   jetmaker->addUeBranch("OffAxisConesR060", off060);
   
  // Run
   chain->Init();
   chain->EventLoop(nevents);
}
