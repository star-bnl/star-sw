/***************************************************************************
 *  
 * Author: M.J. LeVine
 ***************************************************************************
 * Description: common definitions for TRG (dummy placeholder)
 *      
 *
 *   change log
 * 02-Jul-99 MJL add navigation code to get to TRGP bank
 * 08-Jul-99 MJL completely change definition - TRG_Reader is independent 
 *               class which is handed a pointer at the constructor invocation
 *
 ***************************************************************************
 *  
 *
 **************************************************************************/
#ifndef TRG_READER_HH
#define TRG_READER_HH
#include "StDaqLib/GENERIC/EventReader.hh"
#include "StDaqLib/GENERIC/RecHeaderFormats.hh"

// Detector Reader Virtual Class


struct  Bank_TRGP: public Bank
{
  Pointer dummy[6];   //**** PLACE HOLDER for real bank definition ******
  // look at TPC/TPCV2P0.cxx, TPCV2P0.hh for further details
};

class TRG_Reader
{
  friend class EventReader;

public:
  //  move the constructor guts {...} to a .cxx file
  TRG_Reader(EventReader *er, Bank_TRGP *pTRGP){
    pBankTRGP = pTRGP; //copy into class data member for use by other methods
    ercpy = er; // squirrel away pointer eventreader for our friends
    if (!pBankTRGP->test_CRC())  {
    printf("CRC error in TRGP: %s %d\n",__FILE__,__LINE__) ;
  }
    if (pBankTRGP->swap() < 0) {
    printf("swap error in TRGP: %s %d\n",__FILE__,__LINE__) ;
  }
    pBankTRGP->header.CRC = 0;
    //do whatever else needs to be done
  };

  ~TRG_Reader(){}; 


protected:

  // copy of EventReader pointer
  EventReader *ercpy;

  // Bank Pointers
  Bank_DATAP *pBankDATAP;
  Bank_TRGP *pBankTRGP;

};

TRG_Reader *getTRGReader(EventReader *er);

#endif
