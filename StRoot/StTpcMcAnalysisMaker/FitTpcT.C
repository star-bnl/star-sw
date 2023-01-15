/*
   root.exe -q *.H.root FitTpcT.C
 */
void FitTpcT() {
  gROOT->LoadMacro("lBichsel.C");
  lBichsel();
  gROOT->LoadMacro("TpcT.C+");
  Fits();
}
