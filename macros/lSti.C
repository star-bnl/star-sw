void lSti() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("Sti,nodefault");
  bfc(-2,Chain,0,0,0);
}
