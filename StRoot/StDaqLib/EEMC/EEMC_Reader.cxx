#include "EEMC_Reader.hh"
#include "../EVP/emcReader.h" // Has prototypes for emcReader and getEemcTower.
#include <assert.h>
#define MAX_ADC 0xFFF

int EEMC_Reader::getEemcTowerAdc(int crate,int channelJan) {
  int index,channelGerard;

  // Jan and Gerard use the word "channel" in different senses, hence
  // the two int's with names beginning with "channel".

  assert(crate>=0&&crate<=5); // There are only six crates, and their numbering begins at 0.
  assert(channelJan>=0&&channelJan<=159); // Only 160 "channels" per crate.

  /**/ if(crate==3) channelGerard=0;
  else if(crate==4) channelGerard=1;
  else if(crate==5) channelGerard=3;
  else return 0;


  index=channelGerard+30*channelJan;
  // printf("BBB index = %4d\n",index);
  return ::getEemcTower(index);
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
