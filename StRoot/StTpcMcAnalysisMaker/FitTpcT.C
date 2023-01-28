/*
   root.exe -q *.H.root FitTpcT.C
*/
#include "Riostream.h"
void FitTpcT() {
#if defined(__CINT__) && ! defined(__MAKECINT__)
  gROOT->LoadMacro("lBichsel.C");
  lBichsel();
  gROOT->LoadMacro("TpcT.C+");
  Fits();
#else
  cout << " FitTpcT.C does not work with AClick" << endl;
#endif
}
