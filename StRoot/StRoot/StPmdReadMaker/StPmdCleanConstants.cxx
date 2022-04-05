////////////////////////////////////////////////////////////////////////////
//
// $Id: StPmdCleanConstants.cxx,v 1.6 2011/08/17 11:46:07 rashmi Exp $
//
// Description: constants for the Pmd Cleaner and calibration Maker
//
////////////////////////////////////////////////////////////////////////////
#include "StPmdCleanConstants.h"
#include "TMath.h"
ClassImp(PmdClean)
  
  Int_t PmdClean::HitAdcCutOff = 10;
Float_t PmdClean::PmdZ = 538.;
Float_t PmdClean::CellArea = 0.866;

//for whatever counts we are getting on PMD
Float_t PmdClean::OrigHitsMultMin   =    10.;
Float_t PmdClean::OrigHitsMultMax   = 12000.;
Float_t PmdClean::RefMultMin   =    0.;
Float_t PmdClean::RefMultMax   = 2000.;

Float_t PmdClean::EtaMin            =  -4.5;
Float_t PmdClean::EtaMax            =  -1.5;
Float_t PmdClean::PhiMin            =  -TMath::Pi();
Float_t PmdClean::PhiMax            =  TMath::Pi();

// for all the graphs being plotted

Float_t PmdClean::CleanEtaMin        =  -3.7;
Float_t PmdClean::CleanEtaMax        =  -2.3;
Float_t PmdClean::CleanHitsMultMin   =    0.;
Float_t PmdClean::CleanHitsMultMax   = 7000.;
Float_t PmdClean::MipAdcMax          = 500;

Int_t PmdClean::BadChain_y6d25[25]={2,11,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y6d30[25]={2,11,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y6d37[25]={2,11,13,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y6d4041[25]={2,11,13,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y6d64[25]={11,13,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y0d0[25]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y8d0[25]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
//Int_t PmdClean::BadChain_y8d95[25]={2,11,13,14,47,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
//Int_t PmdClean::BadChain_y8d95[25]={2,11,13,14,15,27,31,47,0,0,0,0,0,0,0,0,0,0,0,0};
//Int_t PmdClean::BadChain_y8d89[25]={2,3,5,6,9,11,13,14,15,17,18,21,22,27,29,31,33,37,38,39,40,46,47,0,0};
//These chains removed also gave a bad PMD mult. So one of 46,47 is a problem
//Int_t PmdClean::BadChain_y8d89[25]={2,3,5,6,9,11,13,14,17,18,21,22,27,29,31,33,37,38,39,40,0,0,0,0,0};
//On studying ratio of 8091060 and 8091099 is was found that chain47 (the first 64 channels) is unstable 
// These were further removed from chain47 which is still considered as goodchain
//Int_t PmdClean::BadChain_y8d89[25]={2,3,5,6,9,11,13,14,17,18,21,22,27,29,31,33,37,38,40,0,0,0,0,0,0};
//This was further modified on the basis of ratio fo the channels studies 
Int_t PmdClean::BadChain_y8d89[25]={2,3,5,6,9,11,13,14,17,18,21,22,27,29,31,33,38,39,40,46,47,0,0,0,0};
//Day95 before ratio studies
//Int_t PmdClean::BadChain_y8d95[25]={2,3,5,6,9,11,13,14,15,17,18,21,22,25,27,29,31,33,46,47,0,0,0,0,0};
//Day95 after ratio studies
Int_t PmdClean::BadChain_y8d95[25]={2,3,5,6,9,11,13,14,15,17,18,21,22,25,27,29,31,33,46,47,0,0,0,0,0};
//Int_t PmdClean::BadChain_y8d102[25]={2,6,9,11,12,13,14,15,16,17,18,20,21,22,27,28,43,47,0,0,0,0,0,0};
//After ratio studies to include a few more chiasn like 15,16,28
//Int_t PmdClean::BadChain_y8d102[25]={2,6,9,11,12,13,14,17,18,20,21,22,27,43,47,0,0,0,0,0,0,0,0,0,0};
//CPV was bad so the chains 15,16 were removed once again
Int_t PmdClean::BadChain_y8d102[25]={2,6,9,11,12,13,14,15,16,17,18,20,21,22,27,43,47,0,0,0,0,0,0,0,0};
//gave a bad PMDadc to PMD mult corelation
//Int_t PmdClean::BadChain_y8d108[25]={2,4,5,6,9,11,13,14,15,17,18,21,22,27,31,43,47,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y8d108[25]={2,3,4,5,6,9,11,12,13,14,15,17,18,21,22,27,29,31,33,43,46,47,0,0,0};
Int_t PmdClean::BadChain_y8d116[25]={2,3,5,6,11,13,14,15,17,18,19,21,22,23,27,29,30,31,33,39,46,47,0,0,0};
//Int_t PmdClean::BadChain_y8d122[25]={2,3,5,6,11,13,14,15,17,18,21,22,27,31,33,45,46,47,0,0,0,0,0,0,0};
//August 11
//Int_t PmdClean::BadChain_y8d122[25]={2,3,5,6,11,13,14,15,17,18,21,22,27,31,33,41,45,46,47,0,0,0,0,0,0};
//August 12 put chauin 41 as good again.
Int_t PmdClean::BadChain_y8d122[25]={2,3,5,6,11,13,14,15,17,18,21,22,27,31,33,45,46,47,0,0,0,0,0,0,0};
//August 17 
Int_t PmdClean::BadChain_y8d131[25]={1,2,3,4,5,6,7,9,11,13,14,15,17,18,19,20,21,22,25,29,31,33,39,46,0};
//August 21
// chain 6 is absebnt. Since the number of bnad chains was exccediong the array size of 25, we have remove it from the list since thre is no data for chain6 anywa.
Int_t PmdClean::BadChain_y8d135[25]={1,2,3,4,5,7,9,11,13,14,15,17,18,19,20,21,22,25,27,29,31,33,39,45,46};
//dummy
Int_t PmdClean::BadChain_y8d143[25]={2,5,6,11,13,14,15,17,18,21,22,27,29,31,39,46,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y8d342[25]={5,6,11,14,15,16,17,18,20,21,22,23,24,27,31,46,47,0,0,0,0,0,0,0,0};

Int_t PmdClean::BadChain_y10d0[25]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y10d91[25]={3,5,6,7,8,9,10,11,15,16,17,21,22,23,24,27,29,30,31,32};
Int_t PmdClean::BadChain_y10d100[25]={6,7,8,9,11,15,17,18,21,25,27,32,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y10d105[25]={3,6,7,8,9,11,14,15,17,18,21,22,25,27,32,45,0,0,0,0};
Int_t PmdClean::BadChain_y10d148[25]={3,5,6,7,8,9,11,15,16,17,18,21,22,23,24,27,29,32,43,45,0,0,0,0};

Int_t PmdClean::BadChain_y12d0[30]={0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
Int_t PmdClean::BadChain_y12d114[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,27,37,42,47,0};
Int_t PmdClean::BadChain_y12d172[30]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,27,28,43,0,0};

