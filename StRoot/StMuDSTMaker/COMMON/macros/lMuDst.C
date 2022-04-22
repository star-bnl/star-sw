#include "Riostream.h"
void lMuDst(Int_t opt = -2, const Char_t *input = "", const Char_t *copt = "StEvent,Stu,RMuDst,picoEvt,mysql,tpcDb,magF,nodefault,quiet",const Char_t *tfile =  0,const Char_t *ofile =  0) {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  gROOT->LoadMacro("bfc.C");
  TString Chain(copt);
  TString STAR_VERSION(gSystem->Getenv("STAR_VERSION"));
  if (STAR_VERSION == ".DEV2" || STAR_VERSION.Contains("TFG")) {
    Chain += ",TMVARank";
    gSystem->AddIncludePath(" -D__TFG__VERSION__");
  }
  bfc(opt,Chain,input,ofile,tfile);
  //  gROOT->LoadMacro("FitP_t.h+");
  gSystem->Load("libEG");
#if 0
  gSystem->Load("libKFParticlePerformance");
  gSystem->AddIncludePath(" -I$ROOTROOT/root/tmva/test");
#endif
#endif
}
