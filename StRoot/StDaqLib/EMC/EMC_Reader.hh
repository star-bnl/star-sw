/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for EMC (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to EMCP bank
 * 08-Jul-99 MJL completely change definition - EMC_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 *
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef EMC_READER_HH
#define EMC_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Detector Reader Virtual Class


struct  Bank_EMCP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class EMC_Reader
{
  friend class EventReader;

public:
  //  move the constructor guts {...} to a .cxx file
  EMC_Reader(EventReader *er, Bank_EMCP *pEMCP){
    pBankEMCP = pEMCP; //copy into class data member for use by other methods
    ercpy = er; // squirrel away pointer eventreader for our friends
    if (!pBankEMCP->test_CRC())  {
    printf("CRC error in EMCP: %s %d\n",__FILE__,__LINE__) ;
  }
    if (pBankEMCP->swap() < 0) {
    printf("swap error in EMCP: %s %d\n",__FILE__,__LINE__) ;
  }
    pBankEMCP->header.CRC = 0;
    //do whatever else needs to be done
  };


  ~EMC_Reader(){}; 


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_EMCP *pBankEMCP;

};

EMC_Reader *getEMCReader(EventReader *er);

#endif
