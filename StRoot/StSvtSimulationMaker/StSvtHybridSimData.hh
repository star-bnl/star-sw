/***************************************************************************
 *
 * $Id: StSvtHybridSimData.hh,v 1.3 2001/08/13 15:34:18 bekele Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridSimData.hh,v $
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
class StSvtHybridPixels;

class StSvtHybridSimData: public StSvtHybridData
{
public:
  StSvtHybridSimData(int barrel, int ladder, int wafer, int hybrid, StSvtHybridPixels* mSimDataPixels = 0);

  int setSimHybridData(StSvtHybridPixels* mSimDataPixels);
  int getOffSet(){return mOffSet;};

protected:

  int mOffSet;
  ClassDef(StSvtHybridSimData,1)
};

#endif
