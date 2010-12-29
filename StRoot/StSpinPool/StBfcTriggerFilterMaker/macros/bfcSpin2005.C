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

void bfcSpin2005(int nevents = 100,
		 const char* chainopt = "trs fss y2005h Idst IAna l0 tpcI fcf ftpc Tree logger ITTF Sti VFPPV NoSvtIt NoSsdIt bbcSim tofsim tags emcY2 EEfs evout IdTruth geantout big fzin MiniMcMk eemcDb beamLine clearmem sdt20050727",
		 const char* fzfile = "rcf10100_1_100evts.fzd")
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
  gSystem->Load("StEmcTriggerMaker");
  StEmcTriggerMaker* emctrig = new StEmcTriggerMaker;

  StBfcTriggerFilterMaker* trgfilt = new StBfcTriggerFilterMaker;
  trgfilt->addTrigger(96201);	// 2005 HT1
  trgfilt->addTrigger(96211);	// 2005 HT2
  trgfilt->addTrigger(96221);	// 2005 JP1
  trgfilt->addTrigger(96233);	// 2005 JP2
  chain->AddAfter("eefs",trgfilt);
  chain->AddAfter("eefs",emctrig);

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
