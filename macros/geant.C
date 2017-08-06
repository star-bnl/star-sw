void geant() {
  gROOT->LoadMacro("bfc.C");
  bfc(0,"VMC,VMCAlignment,mysql,sdt20160301,RunG.1,sim_T,nodefault,quiet",0);
}
