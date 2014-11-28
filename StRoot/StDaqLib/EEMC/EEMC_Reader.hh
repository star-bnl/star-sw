/*!
 * \class EEMC_Reader
 * \author Herbert Ward 
 *
 * Main EEMC reader for towers and SMD and ommon definitions for EEMC Bank and EEMC_Reader
*/ 

#ifndef EEMC_READER_HH
#define EEMC_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"
#include "StDaqLib/GENERIC/swaps.hh"


struct Bank_EEMCP: public Bank
{
  struct Pointer EEMCSecPointer[6] ;
};


class EEMC_Reader 
{

public:
  
  EEMC_Reader(EventReader *er, Bank_EEMCP *pEEMCP);///<EEMC_Reader constructor
  ~EEMC_Reader() {}; ///<EEMC_Reader destructor

  u_short *getEemcHeadBlock(int fiber,char type); 
  u_short *getEemcDataBlock(int fiber,char type); 
  
  u_short getEemcHead(int fiber,int channel,char type);
  u_short getEemcData(int fiber,int channel,char type);

  int isEemcBankIn( char type) ;

protected:
  
  // copy of EventReader pointer
  EventReader       *ercpy;
  Bank_EEMCP         *pBankEEMCP;
};

EEMC_Reader *getEEMCReader(EventReader *er);


#endif

/**************************************************************************
 * $Log: 
 *
 **************************************************************************/
