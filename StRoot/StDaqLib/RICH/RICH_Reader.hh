/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for RICH (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to RICHP bank
 * 08-Jul-99 MJL completely change definition - RICH_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef RICH_READER_HH
#define RICH_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"


struct  Bank_RICP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class RICH_Reader
{
  friend class EventReader;

public:
  //  move the constructor guts {...} to a .cxx file
  RICH_Reader(EventReader *er, Bank_RICP *pRICP){
    pBankRICP = pRICP; //copy into class data member for use by other methods
    ercpy = er; // squirrel away pointer eventreader for our friends
    if (!pBankRICP->test_CRC())  {
    printf("CRC error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
    if (pBankRICP->swap() < 0) {
    printf("swap error in RICP: %s %d\n",__FILE__,__LINE__) ;
  }
    pBankRICP->header.CRC = 0;
    //do whatever else needs to be done
  };

  ~RICH_Reader(){}; 


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_RICP *pBankRICP;

};

RICH_Reader *getRICHReader(EventReader *er);
#endif
