void RunJetFinder2012pro(int nevents = 100,
const char* indir = "./",
const char* MuDst = "_e45.MuDst.root",
const char* Jetfile = "test.jets.root",
const char* Uefile = "test.ueoc.root",
const char* Skimfile = "test.skim.root",
const char* geneventfile = "pi+_e45.vz0.run0.genevent.root")
{


    cout<<"MuDst file is "<<MuDst<<endl;
    cout<<"JetTree file is "<<Jetfile<<endl;
    cout<<"SkimTree file is "<<Skimfile<<endl;
    cout<<"Ue file is "<<Uefile<< endl;
    cout<<"GeantEvent file is " <<geneventfile<< endl;
    cout<<"\n " << endl;

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

    gSystem->Load("libMinuit.so");

    gSystem->Load("StFcsUtil");
    gSystem->Load("StFcsDbMaker");

    gSystem->Load( "libVMC.so");
    gSystem->Load( "St_g2t.so" );
    gSystem->Load( "St_geant_Maker.so" );
   
    gSystem->Load( "StarGeneratorUtil.so" );
    gSystem->Load( "StarGeneratorEvent.so" );
    gSystem->Load( "StarGeneratorBase.so" );

    gSystem->Load( "libMathMore.so"   );
    gSystem->Load( "libStarGenEventReader.so" );
    
    
    StChain *chain = new StChain;

      // I/O maker to be used with .geant.root MC truth information
    
//    StIOMaker* ioMaker = new StIOMaker;
//    ioMaker->SetFile(geantFile); //add as input parameter
//    ioMaker->SetIOMode("r");
//    ioMaker->SetBranch("*",0,"0"); // Deactivate all branches
//    ioMaker->SetBranch("genevents",0,"r"); // Activate geant Branch
//    ioMaker->SetBranch("geantBranch",0,"r");   // Activate geant Branch
    
    // Truth Particle information under new StarGenerator format
    if(geneventfile){
        StarGenEventReader *eventreader = new StarGenEventReader("genEvent");
        eventreader->SetInputFile(geneventfile,"genevents","primaryEvent");
    }

    // MuDst reader
    StMuDstMaker* muDstMaker = new StMuDstMaker(0,0,indir, MuDst,"",1000,"MuDst");

    // MuDst DB
    StMuDbReader* muDstDb = StMuDbReader::instance();
    
//    StTriggerFilterMaker* filterMaker = new StTriggerFilterMaker;
//    //JP0
//    filterMaker->addTrigger(380401);
//    //JP1
//    filterMaker->addTrigger(380402);
//    //JP2
//    filterMaker->addTrigger(380403);
//    //AJP
//    filterMaker->addTrigger(380404);

    // star database
     St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");
//     starDb->SetDateTime(20090628,53220);
    
    // FCS DB Maker
    StFcsDbMaker* fcsdbMaker = new StFcsDbMaker;
    
    // Endcap database
    StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

    // star spin database
    StSpinDbMaker* spinDb = new StSpinDbMaker;
    
    // Barrel ADC to energy maker
    StEmcADCtoEMaker* adc = new StEmcADCtoEMaker;
    adc->saveAllStEvent(true);
    
    // FMS no longer in use
    //   StFmsDbMaker* fmsDb = new StFmsDbMaker;
    //   StFmsHitMaker* fmshitMk = new StFmsHitMaker();
    //   StFmsPointMaker* fmsptMk = new StFmsPointMaker("StFmsPointMaker");

//    StTriggerSimuMaker* simuTrig = new StTriggerSimuMaker;
//    simuTrig->useOnlineDB();
//    simuTrig->setMC(0);
//    //simuTrig->useBbc();
//    simuTrig->useBemc();
//    simuTrig->useEemc();
//    simuTrig->bemc->setConfig(StBemcTriggerSimu::kOffline);
//
    StJetSkimEventMaker* skimEventMaker = new StJetSkimEventMaker("StJetSkimEventMaker", muDstMaker, Skimfile);

    // Get Pythia record
    StMCAsymMaker* asym = new StMCAsymMaker;
    
    
//------------------------------------------------------------------------------------
    // StJetMaker2012
    StJetMaker2012* jetmaker = new StJetMaker2012;
    jetmaker->setJetFile(Jetfile);
    jetmaker->setJetFileUe(Uefile);
    
//------------------------------------------------------------------------------------
    //StAnaPars Detect Level
    StAnaPars* anapars12 = new StAnaPars;
    anapars12->useTpc = true;
    anapars12->useBemc = false;
    anapars12->useEemc = false;
    anapars12->useFms = false;
    anapars12->useFcsECal = true;
    anapars12->useFcsHCal = true;
    anapars12->randomSelectorProb = 1.00;

    // The classes available for correcting tower energy for tracks are:
    // 1. StjTowerEnergyCorrectionForTracksMip
    // 2. StjTowerEnergyCorrectionForTracksFraction
    // 3. StjTowerEnergyCorrectionForTracksNull (default: no correction)
    anapars12->setTowerEnergyCorrection(new StjTowerEnergyCorrectionForTracksFraction(1.00));
    
    //TPC Cuts
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
    anapars12->addTpcCut(new StjTrackCutPt(.2,200));
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

    // FCS Cuts
//    anapars12->addFCSEcalemcCut(new StjTowerEnergyCutEt(0.1));
//    anapars12->addFCSHcalhcCut(new StjTowerEnergyCutEt(0.1));
    
    anapars12->addFCSEcalemcCut(new StjTowerEnergyCutEnergy(0.1));
    anapars12->addFCSHcalhcCut(new StjTowerEnergyCutEnergy(0.1));

    // Jet cuts
//    anapars12->addJetCut(new StProtoJetCutPt(1.5,1000000));
    anapars12->addJetCut(new StProtoJetCutEta(-100,100));

//------------------------------------------------------------------------------------

    
    // Set analysis cuts for particle jets branch
    StAnaPars* anaparsParticle = new StAnaPars;
    anaparsParticle->useMonteCarlo = true;

    // MC cuts
    anaparsParticle->addMcCut(new StjMCParticleCutStatus(1)); // final state particles

    // Jet cuts
    anaparsParticle->addJetCut(new StProtoJetCutPt(1.5,1000000));
    anaparsParticle->addJetCut(new StProtoJetCutEta(-100,100));
    
//------------------------------------------------------------------------------------

    // Jet Area
    StFastJetAreaPars *JetAreaPars = new StFastJetAreaPars;
    
    //Anti-kT R=0.6 for run12 jet finding
    StFastJetPars* AntiKtR050Pars = new StFastJetPars;
    AntiKtR050Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
    AntiKtR050Pars->setRparam(0.5); // 0.6
    AntiKtR050Pars->setRecombinationScheme(StFastJetPars::E_scheme);
    AntiKtR050Pars->setStrategy(StFastJetPars::Best);
    AntiKtR050Pars->setPtMin(.1);
    AntiKtR050Pars->setJetArea(JetAreaPars);

    //Anti-kT R=0.5 for run12 jet finding
    StFastJetPars* AntiKtR040Pars = new StFastJetPars;
    AntiKtR040Pars->setJetAlgorithm(StFastJetPars::antikt_algorithm);
    AntiKtR040Pars->setRparam(0.4); // 0.5
    AntiKtR040Pars->setRecombinationScheme(StFastJetPars::E_scheme);
    AntiKtR040Pars->setStrategy(StFastJetPars::Best);
    AntiKtR040Pars->setPtMin(.1);
    AntiKtR040Pars->setJetArea(JetAreaPars);

    jetmaker->addBranch("AntiKtR050NHits12",anapars12,AntiKtR050Pars);
    jetmaker->addBranch("AntiKtR040NHits12",anapars12,AntiKtR040Pars);
    jetmaker->addBranch("AntiKtR050Particle",anaparsParticle,AntiKtR050Pars);
    jetmaker->addBranch("AntiKtR040Particle",anaparsParticle,AntiKtR040Pars);
    StOffAxisConesPars *off050 = new StOffAxisConesPars(0.5);
    StOffAxisConesPars *off060 = new StOffAxisConesPars(0.6);
    jetmaker->addUeBranch("OffAxisConesR050", off050);
    jetmaker->addUeBranch("OffAxisConesR060", off060);

    // Run
    chain->Init();
    chain->EventLoop(nevents);
}
