void lMuDst() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("StEvent,RMuDst,nodefault");
  bfc(-2,Chain,0,0,0);
  //  gROOT->LoadMacro("FitP_t.h+");
  gSystem->Load("libEG");
  //  gSystem->Load("libStKFVertex");
}
