/***************************************************************************
 *
 * $Id: StSvtHybridPixels.hh,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array of Pixels
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixels.hh,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDPIXELS_HH
#define STSVTHYBRIDPIXELS_HH

#include "TArrayF.h"
#include "StSvtHybridObject.hh"

class StSvtHybridPixels: public StSvtHybridObject, public TArrayF
{
public:
  StSvtHybridPixels();
  StSvtHybridPixels(int barrel, int ladder, int wafer, int hybrid, int size = 0, float* x = 0);
  virtual ~StSvtHybridPixels();

  StSvtHybridPixels& operator = (StSvtHybridPixels&);
  StSvtHybridPixels& operator + (StSvtHybridPixels&);

  Float_t getPixelContent(int anode, int time);
  int getNumberOfAnodes() {return mNumberOfAnodes;}
  int getNumberOfTimeBins() {return mNumberOfTimeBins;}
  int getTotalNumberOfPixels() {return mTotalNumberOfPixels;}
  int getPixelIndex(int anode, int time);

protected:

  int mNumberOfAnodes;   // Number of Anodes in one hybrid (= 240)
  int mNumberOfTimeBins; // Number of Time Bins in one hybrid (= 128)
  
  int mTotalNumberOfPixels; // Total Number of Pixels (= 240*128)

  ClassDef(StSvtHybridPixels,1)
};

#endif
