// \class Tonko2Ezt
// \author Jan Balewski
// $Id: Tonko2Ezt.h,v 1.1 2005/04/28 20:54:47 balewski Exp $
// this class is implemented only in online-mode

#ifndef Tonko2Ezt_h 
#define Tonko2Ezt_h

class TObjArray;

#include "StEEmcUtil/EEfeeRaw/EEdims.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"

class Tonko2Ezt { 
 public:
  EztEmcRawData  eETow;
  EztEmcRawData  eESmd;
  Tonko2Ezt();
};
     
#endif

// $Log: Tonko2Ezt.h,v $
// Revision 1.1  2005/04/28 20:54:47  balewski
// start
//


