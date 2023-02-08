void T0offset() {
#if defined(__CINT__) && ! defined(__MAKECINT__)
  gROOT->LoadMacro("lBichsel.C");
  lBichsel();
  gROOT->LoadMacro("TpcT.C+");
  T0Offsets();
#else
  cout << " T0offset.C does not work with AClick" << endl;
#endif
}
