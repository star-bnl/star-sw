/***************************************************************************
 *
 * $Id: StSvtPedSub.h,v 1.1 2000/06/15 20:04:54 caines Exp $
 *
 * Author: Helen Caines
 ***************************************************************************
 *
 * Description: SVT Pedestal Subtraction Code
 *
 ***************************************************************************
 *
 * $Log: StSvtPedSub.h,v $
 * Revision 1.1  2000/06/15 20:04:54  caines
 * Initial versions of sequence adjusting codes
 *
 *
 **************************************************************************/

#ifndef STSVTPEDSUB_H
#define STSVTPEDSUB_H


class StSvtHybridPed;
class StSvtData;
class StSvtHybridData;
class StSvtHybridCollection;

class StSvtPedSub
{
  protected:

  StSvtHybridPed      *mPed;      //!
  StSvtHybridCollection  *mSvtPed;  //!
  
 public: 
  
  StSvtPedSub();
  ~StSvtPedSub();

  int ReadFromFile(char* fileName, StSvtData* fSvtData);
  int SubtractPed( StSvtHybridData* fData, int Index, int PedOffset);
  int Clear();

};


#endif


