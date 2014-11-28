//////////////////////////////////////////////////////////////////////
//
// $Id: StPmdCleanConstants.h 
//
//
// Description: constants for the Pmd Cleaner and calibration Maker

//////////////////////////////////////////////////////////////////////

#ifndef StPmdCleanConstants_h
#define StPmdCleanConstants_h
#include "Rtypes.h"

class PmdClean{

 public:

  static Int_t HitAdcCutOff;
  static Float_t PmdZ;
  static Float_t CellArea;

  //for whatever counts we are getting on PMD

  static Float_t OrigHitsMultMin;
  static Float_t OrigHitsMultMax;
  static Float_t RefMultMin;
  static Float_t RefMultMax;
  static Float_t MipAdcMax;

  static Float_t EtaMin;
  static Float_t EtaMax;
  static Float_t PhiMin;
  static Float_t PhiMax;

  // for various graphs being plotted

  enum{       
    nOrigMultBins   = 40,
      nMultBins   = 50,
      nEtaBins        = 30,
      nPhiBins        = 72
      };
  
  static Float_t CleanEtaMin;
  static Float_t CleanEtaMax;
  static Float_t CleanHitsMultMin;
  static Float_t CleanHitsMultMax;

  //  void BadChains(Int_t,Int_t *);
  static Int_t  BadChain_y0d0[25];
  static Int_t  BadChain_y6d25[25];
  static Int_t  BadChain_y6d30[25];
  static Int_t  BadChain_y6d37[25];
  static Int_t  BadChain_y6d4041[25];
  static Int_t  BadChain_y6d64[25];
  static Int_t BadChain_y8d0[25];
  static Int_t BadChain_y8d89[25];
  static Int_t BadChain_y8d95[25];
  static Int_t BadChain_y8d102[25];
  static Int_t BadChain_y8d108[25];
  static Int_t BadChain_y8d116[25];
  static Int_t BadChain_y8d122[25];
  static Int_t BadChain_y8d131[25];
  static Int_t BadChain_y8d135[25];
  static Int_t BadChain_y8d143[25];
  static Int_t BadChain_y8d342[25];
  static Int_t BadChain_y10d0[25];
  static Int_t BadChain_y10d91[25];
  static Int_t BadChain_y10d100[25];
  static Int_t BadChain_y10d105[25];
  static Int_t BadChain_y10d148[25];

  static Int_t BadChain_y12d0[30];
  static Int_t BadChain_y12d114[30];
  static Int_t BadChain_y12d172[30];

  ClassDef(PmdClean,1)               // macro for rootcint


};

#endif

//////////////////////////////////////////////////////////////////////
