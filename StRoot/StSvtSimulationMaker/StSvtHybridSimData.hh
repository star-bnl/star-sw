/***************************************************************************
 *
 * $Id: StSvtHybridSimData.hh,v 1.4 2003/07/31 19:18:10 caines Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridSimData.hh,v $
 * Revision 1.4  2003/07/31 19:18:10  caines
 * Petrs improved simulation code
 *
 * Revision 1.3  2001/08/13 15:34:18  bekele
 * Debugging tools added
 *
 * Revision 1.2  2001/05/10 04:29:52  caines
 * Change pedestal offset to match real raw data
 *
 * Revision 1.1  2000/11/30 20:47:49  caines
 * First version of Slow Simulator - S. Bekele
 *
 **************************************************************************/

#ifndef STSVTHYBRIDSIMDATA_HH
#define STSVTHYBRIDSIMDATA_HH

#include "StSvtClassLibrary/StSvtHybridData.hh"

class StSequence;
class StSvtHybridPixelsC;

class StSvtHybridSimData: public StSvtHybridData
{
public:
  StSvtHybridSimData(int barrel, int ladder, int wafer, int hybrid, StSvtHybridPixelsC* mSimDataPixels = 0);

  int setSimHybridData(StSvtHybridPixelsC* mSimDataPixels);
  int getOffSet(){return mPedOffset;};

protected:

  int mPedOffset;
  ClassDef(StSvtHybridSimData,1)
};

#endif
