#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;


TBrowser       *b    = 0;
St_geant_Maker *geant= 0;
// reads .fzd file and prints on the screen

// ______________________________________________
void fzd2print(const Int_t Nevents=1, const Char_t *fzfile ="/star/u/spinka/EEMC_sim1/Data/singleGamma1.fzd") {
  Int_t  i=0;
  gSystem->Load("St_base");
  gSystem->Load("StChain");
  gSystem->Load("StarClassLibrary");

  gROOT->LoadMacro("$STAR/StRoot/macros/bfc.C");
  gSystem->Load("StEEmcUtil.so"); 
  bfc(0,"fzin sim_T gen_T nodefault ",fzfile);
  /* note, if BFC crashes due to missing DB time stamp your fzd file is strange
      you can overcome the crash by everwiting event time stamp with 
      the value of your choice.  Add sdt20090101 to the chain options.
      Note, you take responsibility for content of DB for this time stamp.
      Jan B.
  */

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

