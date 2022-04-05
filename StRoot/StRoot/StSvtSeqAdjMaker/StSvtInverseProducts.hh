/***************************************************************************
 *
 * $Id: StSvtInverseProducts.hh,v 1.4 2003/09/02 17:59:08 perev Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtInverseProducts.hh,v $
 * Revision 1.4  2003/09/02 17:59:08  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/11/30 20:45:56  caines
 * Dynamically calc prob values, use database
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/
#ifndef STSVTINVERSEPRODUCTS_HH
#define STSVTINVERSEPRODUCTS_HH

#include "Stiostream.h"
#include "StSvtProbValues.hh"

class StSvtHybridData;

class StSvtInverseProducts
{
public:
   StSvtInverseProducts();
   ~StSvtInverseProducts();

   void SetProbTable(StSvtProbValues* probValue);
   void FindInvProducts(StSvtHybridData* hybridData, int anode, int pedOffSet);

   double GetBuffer(int timeBin);
   void ResetBuffer();

  private:

  double mBuffer[128];      
  double mProbTable[MAX_ADC_COUNTS]; 
  int mMaxCount;
};

#endif
