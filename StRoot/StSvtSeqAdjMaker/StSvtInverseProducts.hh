/***************************************************************************
 *
 * $Id: StSvtInverseProducts.hh,v 1.1 2000/06/15 20:04:54 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtInverseProducts.hh,v $
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 **************************************************************************/
#ifndef STSVTINVERSEPRODUCTS_HH
#define STSVTINVERSEPRODUCTS_HH

#include <fstream.h>

class StSvtHybridData;

class StSvtInverseProducts
{
public:
   StSvtInverseProducts();
   ~StSvtInverseProducts();

   void SetHybridPointer(StSvtHybridData* hybData);
   void FillProbTable(ifstream & iFile);
   void FindInvProducts(int PedOffSet, int Anode);

   double GetBuffer(int timBin);
   void ResetBuffer();

  private:

  StSvtHybridData* mHybridData;    //!      
  double mBuffer[128];      
  double mProbTable[14]; 
};

#endif
