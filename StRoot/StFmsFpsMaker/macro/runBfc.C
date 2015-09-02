TString opt = "in Event l0 trgd ZDCvtx CMuDst eemcDb emcDY2 fmsdat fpsdat fmspoint pp2pp -geant";

void runBfc(const Char_t *file="data/st_fms_16077027_raw_1000002.daq", Int_t nevents=1000000){
  gROOT->LoadMacro("bfc.C");  // Load big "full" chain
  bfc(-1,opt,file);     // Setup but do not init

  // Intializet the chain
  chain->Init();

  // Process events
  chain->EventLoop(nevents);    
}
