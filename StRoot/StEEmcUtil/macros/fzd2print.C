#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;


TBrowser       *b    = 0;
St_geant_Maker *geant= 0;
// reads .fzd file and prints on the screen

// ______________________________________________
void fzd2print(const Int_t Nevents=1, const Char_t *fzfile ="../sim2003/mc_eve2.fzd") {
  Int_t  i=0;
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");

  gROOT->LoadMacro("$STAR/StRoot/macros/bfc.C");
  gSystem->Load("StEEmcUtil.so"); 
  bfc(0,"fzin sim_T gen_T",fzfile);
  EEmcMCData *evIN=new   EEmcMCData;
  
  for (i=1; i<=Nevents; i++ ) {
    chain->Clear();
    if (chain->Make(i)>=kStEOF) break;
    printf("%2d ====================================\n",i);
    int nh=-1;
    nh = evIN->readEventFromChain(chain);
    printf("  actual RAW geant EEMC hits =%d nh\n",nh);
    evIN->print();
   
  }
}

