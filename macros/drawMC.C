void drawMC() {
  gROOT->LoadMacro("MuMc.C+");
  Init(0);
  DrawQA();
  DrawEff();
}
