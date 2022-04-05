//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 5 December 2009
//
// Example macro to use the BFC trigger filter maker.
// Takes as input fzd file, then processes EMC response.
// If event fires selected bits from EMC layer 2 DSM
// (which corresponds to selecting triggers), then
// BFC reconstruction continues. Otherwise, event
// is skipped. Substantial savings in processing
// time and disk space can be achieved when
// reconstruction of complex detectors like the
// TPC is skipped for uninteresting events.
//

class StBFChain;
StBFChain* chain = 0;

void bfcSpin(int nevents = 2000,
	     const char* chainopt = "tpcrs fss y2009a IAna tpcI fcf ftpc Tree logger ITTF Sti VFPPV bbcSim tofsim tags emcY2 EEfs evout -dstout IdTruth geantout big fzin MiniMcMk clearmem eemcDb beamLine sdt20090628.053220",
	     const char* fzfile = "/star/u/mattheww/vmstuff/filterprep/TriggerFilter/testC8__27721041.fzd")
{
  gROOT->LoadMacro("bfc.C");
  bfc(-1,chainopt,fzfile);

  // We want to achieve the following ordering for makers:
  // 1. BBC simulator
  // 2. EMC makers (emcY2+eefs)
  // 3. EEMC slow simulator
  // 4. Pythia event maker
  // 5. Trigger simulator
  // 6. Trigger filter
  // 7. TPC maker

  // Use full BEMC detector
  StEmcSimulatorMaker* emcSim = (StEmcSimulatorMaker*)chain->GetMaker("EmcSimulator");
  emcSim->setCheckStatus(kBarrelEmcTowerId,false);
  emcSim->setMakeFullDetector(kBarrelEmcTowerId,true);
  emcSim->setDoZeroSuppression(kBarrelEmcTowerId,false);

  // Use full EEMC detector
  StEEmcFastMaker* eefs = dynamic_cast<StEEmcFastMaker*>(chain->GetMaker("eefs"));
  assert(eefs);
  eefs->UseFullTower(true);
  //eefs->UseFullPreShower(true);
  //eefs->UseFullSmdu(true);
  //eefs->UseFullSmdv(true);

  // Place TPC chain after EMC makers
  chain->AddAfter("eefs",chain->GetMaker("tpcChain"));

  // Place Pythia maker after GEANT maker
  // and trigger filter after EMC makers
  gSystem->Load("StJetSkimEvent");
  gSystem->Load("StBfcTriggerFilterMaker");

  StPythiaEventMaker* pythia = new StPythiaEventMaker;
  TString pyfile = gSystem->BaseName(fzfile);
  pyfile.ReplaceAll(".fzd",".pythia.root");
  pythia->SetPythiaFile(pyfile);
  chain->AddAfter("geant",pythia);

  // Place trigger simulator after EMC makers
  gSystem->Load("StTriggerUtilities");
  StTriggerSimuMaker* trgsim = new StTriggerSimuMaker;
  trgsim->setMC(1);
  // BBC was not used in Run 9 jet triggers
  //trgsim->useBbc();
  //trgsim->bbc->setSource("StEvent");
  trgsim->useBemc();
  trgsim->bemc->setConfig(StBemcTriggerSimu::kOffline);
  trgsim->useEemc();
  trgsim->eemc->setSource("StEvent");

  gSystem->Load("StMCAsymMaker");
  StBfcTriggerFilterMaker* trgfilt = new StBfcTriggerFilterMaker;
  // The BFC trigger filter will select only JP1, AJP and BHT3 events
  trgfilt->SetJP1();
  trgfilt->SetAJP();
  trgfilt->SetBHT3();
  // Lower all jet patch thresholds by one unit from
  // their values obtained from the database using
  // the current timestamp.
  trgfilt->changeJPThresh(-1);
  chain->AddAfter("eefs",trgfilt);

  chain->AddAfter("eefs",trgsim);

  // Place EEMC slow simulator after EMC makers
  StEEmcSlowMaker* slowSim = new StEEmcSlowMaker;
  slowSim->setSource("StEvent");
  slowSim->setAddPed(true);
  slowSim->setSmearPed(true);
  chain->AddAfter("eefs",slowSim);

  // Run chain
  int istat = chain->Init();
  if (istat) {
    cout << "Chain initialization failed" << endl;
    chain->Fatal(istat,"during Init()");
  }
  // Detailed listing of makers in the chain
  //chain->ls(0);
  cout << "Order of makers in BFC:" << endl;
  StMaker::lsMakers(chain);
  chain->EventLoop(nevents);
}
