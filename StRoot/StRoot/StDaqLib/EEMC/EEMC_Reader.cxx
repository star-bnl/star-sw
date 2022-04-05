#include "EEMC_Reader.hh"
#include "../EVP/emcReader.h" // Has prototypes for emcReader and getEemcTower.
#include <assert.h>

using namespace OLDEVP;

int EEMC_Reader::isEemcBankIn( char type) {
  switch(type) {
  case 'T': // tower crates
    return emc.etow_in;
  case 'S': // smd/pre/post  crates
    return emc.esmd_in;
  default:
    assert(2==3);
  }
  return 0;
}


u_short *EEMC_Reader::getEemcHeadBlock(int fiber, char type) {

  switch(type) {
  case 'T': // tower crates
    assert(fiber>=0 && fiber <ETOW_MAXFEE);
    return emc.etow_pre[fiber];
    break;
  case 'S': // smd/pre/post  crates
    assert(fiber>=0 && fiber <=ESMD_MAXFEE);
    return emc.esmd_pre[fiber];
    break;
  default:
    assert(2==3);
  }

}


u_short *EEMC_Reader::getEemcDataBlock(int fiber, char type) {

  switch(type) {
  case 'T': // tower crates
    assert(fiber>=0 && fiber <ETOW_MAXFEE);
    return emc.etow[fiber];
    break;
  case 'S': // smd/pre/post  crates
    assert(fiber>=0 && fiber <=ESMD_MAXFEE);
    return emc.esmd[fiber];
    break;
  default:
    assert(2==3);
  }

}




u_short EEMC_Reader::getEemcHead(int fiber,int channel, char type) {
  u_short val=0;
  switch(type) {
  case 'T': // tower crates
    assert(channel>=0 && channel <ETOW_PRESIZE);
    assert(fiber>=0 && fiber <ETOW_MAXFEE);
    val=emc.etow_pre[fiber][channel];
    break;
  case 'S': // smd/pre/post  crates
    assert(channel>=0 && channel <ESMD_PRESIZE);
    assert(fiber>=0 && fiber <=ESMD_MAXFEE);
    val=emc.esmd_pre[fiber][channel];
    break;
  default:
    assert(2==3);
  }
  return val;

}


u_short EEMC_Reader::getEemcData(int fiber,int channel, char type) {
  u_short val=0;
  switch(type) {
  case 'T': // tower crates
    assert(channel>=0 && channel <ETOW_DATSIZE);
    assert(fiber>=0 && fiber <ETOW_MAXFEE);
    val=emc.etow[fiber][channel];
    break;
  case 'S': // smd/pre/post  crates
    assert(channel>=0 && channel <ESMD_DATSIZE);
    assert(fiber>=0 && fiber <=ESMD_MAXFEE);
    val=emc.esmd[fiber][channel];
    break;
  default:
    assert(2==3);
  }
  return val;

}



EEMC_Reader::EEMC_Reader(EventReader *er, Bank_EEMCP *pEEMCP)
{
  Bank_DATAP *datap = (Bank_DATAP*) er->getDATAP();

#ifndef NEW_DAQ_READER_DAQLIB
  OLDEVP::emcReader((char*)datap); // ::emcReader saves data in a static global, and gives it
                             // out via ::getEemcTower (see above)
  //  LTime = (time->tm_hour*100 + time->tm_min)*100 + time->tm_sec;
#else 
   daqReader *rdr=er->getDaqReader(); assert(rdr);
  ::emcReader((char*)rdr); // call the "event pool" code
#endif
  pBankEEMCP = pEEMCP; //copy into class data member for use by other methods
  ercpy = er; // squirrel away pointer eventreader for our friends

  // Protection against NULL pointer
  if (!pEEMCP) return;

  if (!pBankEEMCP->test_CRC()) printf("CRC error in EEMCP: %s %d\n",__FILE__,__LINE__) ;

  ////////////////////////////////////////////////////////////////////////////////
  // Swapping is done in ::emcReader().  This is not usual StDaqLib practice,
  // but it does allow maximum re-use of DAQ's Event Pool code
  // if (pBankEEMCP->swap() < 0)  printf("swap error in EEMCP: %s %d\n",__FILE__,__LINE__) ;
  ////////////////////////////////////////////////////////////////////////////////

  pBankEEMCP->header.CRC = 0;
}


