#include <iostream.h>

class    EEmcMCData;

// ______________________________________________
void fzd2muDst(const Int_t Nevents=1, const Char_t *fzfile ="aa.fzd") {
  Int_t  i=0;
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");

  gROOT->LoadMacro("./bfc.C");
  //gROOT->LoadMacro("/star/u/laue/afsWork/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  gROOT->LoadMacro("$STAR/StRoot/StMuDSTMaker/COMMON/macros/loadSharedLibraries.C");
  loadSharedLibraries();
  gSystem->Load("StBlankStEventMaker");
  gSystem->Load("StEEmcUtil");  
  gSystem->Load("StEEmcSimulatorMaker");  
  bfc(0,"fzin sim_T gen_T tree",fzfile);// just setup the chain
  
  for (i=1; i<=Nevents; i++ ) {
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    printf("%2d ====================================\n",i);   
   }
}

