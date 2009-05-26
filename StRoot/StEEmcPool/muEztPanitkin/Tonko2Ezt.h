// \class Tonko2Ezt
// \author Jan Balewski
// $Id: Tonko2Ezt.h,v 1.3 2009/05/26 08:45:12 ogrebeny Exp $
// this class is implemented only in online-mode

#ifndef Tonko2Ezt_h 
#define Tonko2Ezt_h

class TObjArray;

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

class Tonko2Ezt { 
 public:
  Bool_t eETowPresent;
  EztEmcRawData  eETow;
  Bool_t eESmdPresent;
  EztEmcRawData  eESmd;
  Tonko2Ezt(char *rdr = 0);
};
     
#endif

// $Log: Tonko2Ezt.h,v $
// Revision 1.3  2009/05/26 08:45:12  ogrebeny
// Bug fix to disable EEMC plots if the detector is not in the run
//
// Revision 1.2  2009/01/24 01:14:35  ogrebeny
// Now uses new DAQ reader
//
// Revision 1.1  2005/04/28 20:54:47  balewski
// start
//


