#include "Riostream.h"
void lMuDst(Int_t opt = -2, const Char_t *input = "", const Char_t *copt = "StEvent,RMuDst,mysql,tpcDb,magF,nodefault,CorrX",const Char_t *tfile =  0) {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  gROOT->LoadMacro("bfc.C");
  TString Chain(copt);
  if (TString(gSystem->Getenv("STAR_VERSION")) == ".DEV2") Chain += ",TMVARank";
  bfc(opt,Chain,input,0,tfile);
  //  gROOT->LoadMacro("FitP_t.h+");
  gSystem->Load("libEG");
  gSystem->Load("libKFParticlePerformance");
  gSystem->AddIncludePath(" -I$ROOTROOT/root/tmva/test");
#endif
}
