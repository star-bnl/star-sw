/***************************************************************************
 *
 * $Id: StSvtHybridPixelsD.hh,v 1.1 2003/07/31 19:04:53 caines Exp $
 *
 * Author: Petr Chaloupka
 ***************************************************************************
 *
 * Description: SVT Hybrid Array of Pixels with double precision
 *
 ***************************************************************************/


#ifndef STSVTHYBRIDPIXELSD_HH
#define STSVTHYBRIDPIXELSD_HH

#include "TArrayD.h"
#include "StSvtHybridObject.hh"

class StSvtHybridPixelsD: public StSvtHybridObject, public TArrayD
{
public:
  StSvtHybridPixelsD();
  StSvtHybridPixelsD(int barrel, int ladder, int wafer, int hybrid, int size = 0, double* x = 0);
  virtual ~StSvtHybridPixelsD();

  StSvtHybridPixelsD& operator = (StSvtHybridPixelsD&);
  StSvtHybridPixelsD& operator + (StSvtHybridPixelsD&);

  double getPixelContent(int anode, int time);
  void addToPixel(int anode, int time, char x);
  void addToPixel(int index, char x);
  void addToPixel(int anode, int time, int x);
  void addToPixel(int index, int x);
  void addToPixel(int anode, int time, double x);
  void addToPixel(int index, double x);

  int getNumberOfAnodes() {return mNumberOfAnodes;}
  int getNumberOfTimeBins() {return mNumberOfTimeBins;}
  int getTotalNumberOfPixels() {return mTotalNumberOfPixels;}
  int getPixelIndex(int anode, int time);

  void setPedOffset(int offset){mPedOffset = offset;}
  int  getPedOffset(){return mPedOffset;}

  void reset();

protected:

  int mNumberOfAnodes;   // Number of Anodes in one hybrid (= 240)
  int mNumberOfTimeBins; // Number of Time Bins in one hybrid (= 128)
  
  int mTotalNumberOfPixels; // Total Number of Pixels (= 240*128)

  int mPedOffset;

  ClassDef(StSvtHybridPixelsD,1)
};

#endif
