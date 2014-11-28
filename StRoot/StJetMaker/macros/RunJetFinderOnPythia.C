//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 17 Dec 2009
//

void RunJetFinderOnPythia(int nevents = 1000,
			  const char* pythiafile = "/star/u/pibero/iucf/embedding/P10ic/Jet_pp200_2009/pythia/pt11_15_10146091_1.pythia.root",
			  const char* jetfile = "jets.root")
{
  // Load shared libraries
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

  cout << "Loading shared libraries done" << endl;

  // Create chain
  StChain* chain = new StChain;

  // STAR database: Dummy. Not used for PYTHIA jets, but required by StJetMaker.
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");
  starDb->SetDateTime(20090628,53220); // Run 10179006

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Read Pythia records
  St_pythia_Maker* pythia = new St_pythia_Maker;
  pythia->SetFile(pythiafile);

  // Pythia4pMaker
  StPythiaFourPMaker* pythia4pMaker = new StPythiaFourPMaker;

  // Instantiate the JetMaker
  StJetMaker2009* jetmaker = new StJetMaker2009;
  jetmaker->setJetFile(jetfile);

  //------------------------------------------------------------------------------------

  // Set analysis cuts for particle jets branch
  StAnaPars* anaparsParticle = new StAnaPars;
  anaparsParticle->useMonteCarlo = true;

  // MC cuts
  anaparsParticle->addMcCut(new StjMCParticleCutStatus(1)); // final state particles

  // Jet cuts
  anaparsParticle->addJetCut(new StProtoJetCutPt(3,200));
  anaparsParticle->addJetCut(new StProtoJetCutEta(-100,100));
    
  //------------------------------------------------------------------------------------

  // Set analysis cuts for parton jets branch
  StAnaPars* anaparsParton = new StAnaPars;
  anaparsParton->useMonteCarlo = true;

  // MC cuts
  anaparsParton->addMcCut(new StjMCParticleCutParton);

  // Jet cuts
  anaparsParton->addJetCut(new StProtoJetCutPt(3,200));
  anaparsParton->addJetCut(new StProtoJetCutEta(-100,100));

  //------------------------------------------------------------------------------------

  for (float coneRadius = 0.4; coneRadius < 0.8; coneRadius += 0.1) {
    // Set cone jet finder parameters
    StConePars* conepars = new StConePars;
    conepars->setGridSpacing(105,-3.0,3.0,120,-TMath::Pi(),TMath::Pi());
    conepars->setConeRadius(coneRadius);
    conepars->setSeedEtMin(0.5);
    conepars->setAssocEtMin(0.1);
    conepars->setSplitFraction(0.5);
    conepars->setPerformMinimization(true);
    conepars->setAddMidpoints(true);
    conepars->setRequireStableMidpoints(true);
    conepars->setDoSplitMerge(true);
    conepars->setDebug(false);

    int iConeRadius = coneRadius*100;
    jetmaker->addBranch(Form("ParticleConeJetsR%03d",iConeRadius),anaparsParticle,conepars);
    jetmaker->addBranch(Form("PartonConeJetsR%03d",iConeRadius),anaparsParton,conepars);
  }

  //------------------------------------------------------------------------------------

  // Run
  chain->Init();
  chain->EventLoop(nevents);
}
