void lMuDstK() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("StEvent,genvtx,CMuDst,nodefault");
  bfc(-2,Chain,0,0,0);
  gSystem->Load("libEG");
  gSystem->Load("libStKFVertex");
}
