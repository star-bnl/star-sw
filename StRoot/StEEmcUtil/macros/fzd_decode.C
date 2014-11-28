//=======================================================================
// owner: Oleg Rogachevsky
// revised: Piotr A Zolnierczuk
//=======================================================================
//
#include <iostream.h>

class    St_geant_Maker;
class    EEmcMCData;


TBrowser       *b    = 0;
St_geant_Maker *geant= 0;

// _____________________________________________________________________
void fzd_decode(const Int_t Nevents=1, const Char_t *fzfile ="muon.fzd")
{
  Int_t  i=0;

  gSystem->Load("EEmc.so");  //

  gROOT->LoadMacro("bfc.C");
  bfc(0,"fzin sim_T gen_T",fzfile);

  EEmcMCData      *ev     = new EEmcMCData;
 

  char *hitBuf = new char[16384];

  for (i=1; i<=Nevents; i++ ) {


    //chain->Clear();
    //if (chain->Make(i)>=kStEOF) break;
    //printf("%2d ====================================\n",i);
    //St_g2t_event *g2t_event = (St_g2t_event *)chain->FindObject("g2t_event");
    //assert(g2t_event);

    //St_g2t_ctf_hit *emc_hit = (St_g2t_ctf_hit *) chain->FindObject("g2t_eem_hit");

    //if (emc_hit!=0) { 
    int nh;
    if ( (nh = ev->readEventFromChain(chain,i)) >0) {
      cerr << "actual hits " << nh << endl;
      ev->print();
      //EEmcMCData      *evcopy = new EEmcMCData(*ev);
      //int nbw = ev->write(hitBuf,4096);
      //cerr << "actual writ " << nbw << " bytes " << endl;
      //int nbr = evcopy->read(hitBuf,nbw);
      //cerr << "actual read " << nbr << " bytes " << endl;
      //evcopy->print();
    } else {
      cerr << "EEMC/CTF hits not found" << endl;
    }
  }
}

