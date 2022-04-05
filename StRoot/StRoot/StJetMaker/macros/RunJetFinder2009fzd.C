//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 19 Jan 2012
//
// Run jet finder on Zebra file
//

void RunJetFinder2009fzd(int nevents = 1000, const char* fzdfile = "../eliza14/SL10c_emb/10180030/pt2_3_10180030_1.fzd", const char* jetfile = "jets.root")
{
  gROOT->Macro("loadMuDst.C");
  gROOT->Macro("LoadLogger.C");
  //gROOT->Macro("$STAR/StRoot/macros/loadMuDst.C");
  //gROOT->Macro("$STAR/StRoot/macros/LoadLogger.C");

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
  //gSystem->Load("libfastjet.so");
  gSystem->Load("fastjet-install/lib/libfastjet.so");
  gSystem->Load("StJetFinder");
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StJets");
  gSystem->Load("StJetEvent");
  gSystem->Load("StJetMaker");
  gSystem->Load("StEEmcSimulatorMaker");

  gSystem->Load("St_g2t");
  gSystem->Load("St_geant_Maker");

  StChain* chain = new StChain;

  St_geant_Maker* geant = new St_geant_Maker;
  geant->SetInputFile(fzdfile);

  // StMcEvent maker
  StMcEventMaker* mcEventMaker = new StMcEventMaker;
  mcEventMaker->doPrintEventInfo = false;
  mcEventMaker->doPrintMemoryInfo = false;

  // star database
  St_db_Maker* starDb = new St_db_Maker("StarDb","MySQL:StarDb");
  starDb->SetDateTime(20111231,0);

  // Endcap database
  StEEmcDbMaker* eemcDb = new StEEmcDbMaker;

  // Jet maker
  StJetMaker2009* jetmaker = new StJetMaker2009;
  jetmaker->setJetFile(jetfile);

  // Set analysis cuts for particle jets branch
  StAnaPars* anaparsParticle = new StAnaPars;
  anaparsParticle->useMonteCarlo = true;

  // MC cuts
  anaparsParticle->addMcCut(new StjMCParticleCutStatus(1)); // final state particles

  // Jet cuts
  anaparsParticle->addJetCut(new StProtoJetCutPt(3,200));
  anaparsParticle->addJetCut(new StProtoJetCutEta(-100,100));

  // Set analysis cuts for parton jets branch
  StAnaPars* anaparsParton = new StAnaPars;
  anaparsParton->useMonteCarlo = true;

  // MC cuts
  anaparsParton->addMcCut(new StjMCParticleCutParton);

  // Jet cuts
  anaparsParton->addJetCut(new StProtoJetCutPt(3,200));
  anaparsParton->addJetCut(new StProtoJetCutEta(-100,100));

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

    jetmaker->addBranch(Form("ParticleConeJets%03d",iConeRadius),anaparsParticle,conepars);
    jetmaker->addBranch(Form("PartonConeJets%03d",iConeRadius),anaparsParton,conepars);
  }

  chain->Init();
  chain->EventLoop(nevents);
}
