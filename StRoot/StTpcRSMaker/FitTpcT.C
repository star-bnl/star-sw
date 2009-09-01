void FitTpcT() {
  gInterpreter->ProcessLine(".L ~/macros/TpcT.C+");
  Fits();
}
