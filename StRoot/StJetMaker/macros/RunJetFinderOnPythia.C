//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 17 Dec 2009
//

void RunJetFinderOnPythia(int nevents = 2000,
			  const char* pyfile = "/star/data05/scratch/pibero/SpinSimulation/pp200/pythia_6422/perugia_0/pt2_3gev/jet_pp200_pythia_6422_perugia_0_pt2_3gev_2000evts_185.pythia.root",
			  const char* jetfile = "Jets.root")
{
  // Load shared libraries
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
  gSystem->Load("libfastjet.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  cout << "Loading shared libraries done" << endl;

  // Create chain
  StChain* chain = new StChain;

  // Read Pythia records
  St_pythia_Maker* pythia = new St_pythia_Maker;
  pythia->SetFile(pyfile);

  // Pythia4pMaker
  StPythiaFourPMaker* pythia4pMaker = new StPythiaFourPMaker;

  // Instantiate the JetMaker
  StJetMaker* jetMaker = new StJetMaker("jetMaker",0,jetfile);
  jetMaker->SetTreeWriter(new StjeJetEventTreeWriter(jetfile));

  // Set the analysis cuts for pythia clustering: (See StJetMaker/StppJetAnalyzer.h -> class StppAnaPars)
  StppAnaPars* pythia_apars = new StppAnaPars;
  pythia_apars->setFlagMin(0);
  pythia_apars->setNhits(0);
  pythia_apars->setCutPtMin(0.0001);
  pythia_apars->setAbsEtaMax(5.0);
  pythia_apars->setJetPtMin(3.0);
  pythia_apars->setJetEtaMax(5.0);
  pythia_apars->setJetEtaMin(0);
  pythia_apars->setJetNmin(0);
    
  StConePars* pythia_cpars = new StConePars;
  pythia_cpars->setGridSpacing(105,-3,3,120,-TMath::Pi(),TMath::Pi()); // including EEMC
  pythia_cpars->setConeRadius(0.7);
  pythia_cpars->setSeedEtMin(0.5);
  pythia_cpars->setAssocEtMin(0.1);
  pythia_cpars->setSplitFraction(0.5);
  pythia_cpars->setPerformMinimization(true);
  pythia_cpars->setAddMidpoints(true);
  pythia_cpars->setRequireStableMidpoints(true);
  pythia_cpars->setDoSplitMerge(true);
  pythia_cpars->setDebug(false);

  jetMaker->addAnalyzer(pythia_apars,pythia_cpars,pythia4pMaker,"PythiaConeJets");

  // Run
  chain->Init();
  chain->EventLoop(nevents);
  return;

  // Event loop
  for (int iEvent = 1; iEvent <= nevents; ++iEvent) {
    chain->Clear();
    int status = chain->Make(iEvent);
    if (status % 10 == kStEOF || status % 10 == kStFatal) break;
    TTree* jetTree = jetMaker->tree();
    StJetEvent* jetEvent = *(StJetEvent**)jetTree->GetBranch("PythiaConeJets")->GetAddress();
    cout << "run = " << jetEvent->runId() << ", event = " << jetEvent->eventId() << endl;
    cout << "subprocessId = " << pythia->GetEvent()->processId() << endl;
    cout << "njets = " << jetEvent->numberOfJets() << endl;
    // Jet loop
    for (int i = 0; i < jetEvent->numberOfJets(); ++i) {
      StJetCandidate* jet = jetEvent->jet(i);
      cout << "jet #" << i << ": pt=" << jet->pt() << ", eta=" << jet->eta() << ", phi=" << jet->phi()
	   << ", npart=" << jet->numberOfParticles() << endl;
      for (int j = 0; j < jet->numberOfParticles(); ++j) {
	StJetParticle* part = jet->particle(j);
	cout << "part #" << j << ": pdg=" << part->pdg() << ", name=" << part->name()
	     << ", pt=" << part->pt() << ", eta=" << part->eta() << ", phi=" << part->phi() << endl;
      }
    } // End jet loop
  } // End event loop
}
