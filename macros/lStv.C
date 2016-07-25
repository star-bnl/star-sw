void lStv() {
  gROOT->LoadMacro("bfc.C");
  TString Chain("SsdDb,Stu,SvtDb,Stv,nodefault");
  bfc(-2,Chain,0,0,0);
}
