/*
  root.exe *Fit.root DrawAllF.C
*/
void DrawAllF() {
  gROOT->LoadMacro("DrawList.C+");
  DrawFAll();
}
