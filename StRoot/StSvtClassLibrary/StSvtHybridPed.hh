/***************************************************************************
 *
 * $Id: StSvtHybridPed.hh,v 1.1 2000/06/15 15:45:54 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pedestal class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPed.hh,v $
 * Revision 1.1  2000/06/15 15:45:54  caines
 * Add Pedestal Class for SVT
 *
 **************************************************************************/

#ifndef STSVTHYBRIDPED_HH
#define STSVTHYBRIDPED_HH

#include "StSvtEnumerations.hh"
#include "StSvtHybridPixels.hh"

class StSvtHybridPed: public StSvtHybridPixels
{
public:
  StSvtHybridPed();
  StSvtHybridPed(int barrel, int ladder, int wafer, int hybrid, pedestalType type=kTime);

  pedestalType getType(){return mType;}
  void setType(pedestalType type){mType = type;}

private:  
  pedestalType mType;  // Pedestal Type (kCapacitor or kTime)

protected:

  ClassDef(StSvtHybridPed,1)
};

#endif
