#include "EEMC_Reader.hh"
#include "../EVP/emcReader.h" // Has prototypes for emcReader and getEemcTower.
#include <assert.h>
#define MAX_ADC 0xFFF

// modified by Piotr A Zolnierczuk
int EEMC_Reader::getEemc2004(int crate,int channel) { // To resolve confusion about "channel", see
                                                      // comments in getEemcTowerAdc.
  //int index;
  if(crate>=1&&crate<=6) { // Tower portion of EEMC.
    //index = (crate-1) + 6 * channel ;
    //return ::getEemcTower(index); // emc.etow[index]
    return emc.etow[crate-1][channel]; 
  } else if(crate>=84&&crate<=99) { // MAPMT portion of EEMC.
    //index = crate - 82 + 30 * channel ;
    //return ::getEemcMapmt(index); // emc.esmd[index]
    return emc.esmd[crate-82][channel]; 
  } else {
    return 0;
  }
  
}
int EEMC_Reader::getEemcTowerAdc(int crate,int channelJan) {
  //int index,channelGerard;
  int channelGerard;

  // Jan and Gerard use the word "channel" in different senses, hence
  // the two int's with names beginning with "channel".

  assert(crate>=0&&crate<=5); // There are only six crates, and their numbering begins at 0.
  assert(channelJan>=0&&channelJan<=159); // Only 160 "channels" per crate.

  /**/ if(crate==3) channelGerard=0;
  else if(crate==4) channelGerard=1;
  else if(crate==5) channelGerard=3;
  else return 0;

  //index=channelGerard+30*channelJan;
  // printf("BBB index = %4d\n",index);
  //return ::getEemcTower(index);
  return emc.etow[channelGerard][channelJan];
  
}

// added by Piotr A Zolnierczuk
int EEMC_Reader::getEemc(int crate,int chan, int mapping) {
  // a lousy crate <-> fee data index mapping 
  if(mapping<0) mapping=kFY2004;
  switch(mapping) {
  case kFY2003:
    switch(crate) {
    case  3: return emc.etow[0][chan]; break;
    case  4: return emc.etow[1][chan]; break;
    case  5: return emc.etow[3][chan]; break; // redundant breaks
    default: break;
    }
    break;
  case kFY2004:
    if( 1<=crate && crate<= 6) return emc.etow[crate- 1][chan];
    if(84<=crate && crate<=99) return emc.esmd[crate-82][chan];
    break;
  case kBEYOND: break; // not yet there
  default:      break;
  }
  return -1;
}
  

EEMC_Reader::EEMC_Reader(EventReader *er, Bank_EEMCP *pEEMCP) 
{
  Bank_DATAP *datap = (Bank_DATAP*) er->getDATAP();
  ::emcReader((char*)datap); // ::emcReader saves data in a static global, and gives it
                             // out via ::getEemcTower (see above)

  pBankEEMCP = pEEMCP; //copy into class data member for use by other methods
  ercpy = er; // squirrel away pointer eventreader for our friends
  
  if (!pBankEEMCP->test_CRC()) printf("CRC error in EEMCP: %s %d\n",__FILE__,__LINE__) ;

  ////////////////////////////////////////////////////////////////////////////////
  // Swapping is done in ::emcReader().  This is not usual StDaqLib practice,
  // but it does allow maximum re-use of DAQ's Event Pool code 
  // if (pBankEEMCP->swap() < 0)  printf("swap error in EEMCP: %s %d\n",__FILE__,__LINE__) ;
  ////////////////////////////////////////////////////////////////////////////////
 
  pBankEEMCP->header.CRC = 0;
}
