void loadSharedLibraries() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("StEvent,CMuDst,nodefault");
  bfc(-2,Chain,0,0,0);
 }
