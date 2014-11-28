/*
   root.exe -q *.H.root FitTpcT.C
 */
void FitTpcT() {
  gROOT->LoadMacro("TpcT.C+");
  Fits();
}
