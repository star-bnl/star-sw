#include "Riostream.h"
void lHftTree() {
#if !defined(__CINT__)
  std::cout << "This code cannot be compiled" << std::endl;
#else
  gROOT->LoadMacro("bfc.C");
  TString Chain("in,StEvent,mysql,libPhysics,db,StarMagField,MagF,pxlDb,istDb,HftMatTree,nodefault,corrX");
  bfc(-2,Chain,0,0,0);
  //  gROOT->LoadMacro("FitP_t.h+");
  gSystem->Load("libEG");
  //  gSystem->Load("libStKFVertex");
  gSystem->AddIncludePath(" -I$ROOTROOT/root/tmva/test");
#if 0
  StMaker *muDst = chain->Maker("MuDst");
  StMaker *dbMk  = chain->Maker("db");
  chain->AddAB("db",muDst,-1); // move  MuDst befor db
  StMaker::lsMakers(chain);
#endif
#endif
}
