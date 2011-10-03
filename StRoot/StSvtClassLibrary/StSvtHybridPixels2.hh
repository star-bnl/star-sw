/***************************************************************************
 *
 * $Id: StSvtHybridPixels2.hh,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array of Pixels used for 2 order pedestal correction
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixels2.hh,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDPIXELS2_HH
#define STSVTHYBRIDPIXELS2_HH

#include "StSvtHybridObject.hh"

class StObjArray;
class StSvtHybridPixels;

class StSvtHybridPixels2: public StSvtHybridObject
{
public:
  StSvtHybridPixels2(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtHybridPixels2();

  void setSvtHybridPixels(StSvtHybridPixels* pixels, int time2) {mPixels->AddAt(pixels,time2);}
  StSvtHybridPixels* getSvtHybridPixels(int time2) {return (StSvtHybridPixels*)mPixels->At(time2);}

  int getNumberOfCapacitors(){return mNumberOfCapacitors;}

protected:

  int mNumberOfCapacitors;  // Number of Capacitors in the SCA (same as time bins)

  StObjArray* mPixels;

  ClassDef(StSvtHybridPixels2,1)
};

#endif
