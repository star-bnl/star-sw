/*
   root.exe -q *.H.root TpcTQA.C
 */
void QATpcT() {
  gROOT->LoadMacro("TpcT.C+");
  TpcTQA();
}
