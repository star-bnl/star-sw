/***************************************************************************
 *  
 * Authors: Herbert Ward, 
 ***************************************************************************
 * Description: common definitions for EEMC Bank and EEMC_Reader
 ***************************************************************************
 *  
 *
 **************************************************************************/

/*!\class EEMC_Reader
\author Herbert Ward 

Main EEMC reader for towers and SMD.
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

struct Bank_EEMCSECP: public Bank
{
  struct Pointer FiberPointer[8] ; /* No of fibere to daq from each subdetector, can be maximum 8 */
};

struct Bank_EEMCRBP: public Bank
{
  struct Pointer EEMCADCR;  /* RAW DATA */
  struct Pointer EEMCADCD ; /* one tower block*/
  struct Pointer EEMCPEDR; 
  struct Pointer EEMCRMSR ; /* one tower block*/
};




class EEMC_Reader 
{
  void              ProcessEvent(const Bank_EEMCP *EmcPTR);///<Process EEMC (tower+SMD) event
  
public:
  
  EEMC_Reader(EventReader *er, Bank_EEMCP *pEEMCP);///<EEMC_Reader constructor
  ~EEMC_Reader() {}; ///<EEMC_Reader destructor

  u_short *getEemcHeadBlock(int fiber,char type); 
  u_short *getEemcDataBlock(int fiber,char type); 
  
  u_short getEemcHead(int fiber,int channel,char type);
  u_short getEemcData(int fiber,int channel,char type);

protected:
  
  // copy of EventReader pointer
  EventReader       *ercpy;
  Bank_EEMCP         *pBankEEMCP;
};

EEMC_Reader *getEEMCReader(EventReader *er);


#endif
