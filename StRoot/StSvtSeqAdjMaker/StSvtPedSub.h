/***************************************************************************
 *
 * $Id: StSvtPedSub.h,v 1.2 2000/08/21 12:57:31 caines Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: SVT Pedestal Subtraction Code
 *
 ***************************************************************************
 *
 * $Log: StSvtPedSub.h,v $
 * Revision 1.2  2000/08/21 12:57:31  caines
 * Now opens and reads in ped using CalibMaker
 *
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 *
 **************************************************************************/

#ifndef STSVTPEDSUB_H
#define STSVTPEDSUB_H


class StSvtHybridPed;
class StSvtHybridData;
class StSvtHybridCollection;

class StSvtPedSub
{
  protected:

  StSvtHybridPed      *mPed;      //!
  StSvtHybridCollection  *mSvtPed;  //!
  
 public: 
  
  StSvtPedSub(StSvtHybridCollection *PedPointer);
  ~StSvtPedSub();

  int SubtractPed( StSvtHybridData* fData, int Index, int PedOffset);


};


#endif


