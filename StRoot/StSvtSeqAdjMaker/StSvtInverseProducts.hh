/***************************************************************************
 *
 * $Id: StSvtInverseProducts.hh,v 1.2 2000/10/02 13:48:10 caines Exp $
 *
 * Author: 
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************
 *
 * $Log: StSvtInverseProducts.hh,v $
 * Revision 1.2  2000/10/02 13:48:10  caines
 * Adjusting donw hybrid by hybrid
 *
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
   void FillProbTable(ifstream & iFile, int TotalNumberOfHybrids);
   void FindInvProducts(int HybIndex, int PedOffSet, int Anode);

   double GetBuffer(int timBin);
   void ResetBuffer();

  private:

  StSvtHybridData* mHybridData;    //!      
  double mBuffer[128];      
  double **mProbTable; 
};

#endif
