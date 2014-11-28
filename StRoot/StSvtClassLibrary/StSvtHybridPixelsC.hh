/***************************************************************************
 *
 * $Id: StSvtHybridPixelsC.hh,v 1.1 2001/08/16 21:02:04 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Array of Pixels
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixelsC.hh,v $
 * Revision 1.1  2001/08/16 21:02:04  munhoz
 * changing StObjArray to StStrArray. StSvtConfig reestructured. New classes for geometry DB
 *
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDPIXELSC_HH
#define STSVTHYBRIDPIXELSC_HH

#include "TArrayC.h"
#include "StSvtHybridObject.hh"

class StSvtHybridPixelsC: public StSvtHybridObject, public TArrayC
{
public:
  StSvtHybridPixelsC();
  StSvtHybridPixelsC(int barrel, int ladder, int wafer, int hybrid, int size = 0, unsigned char* x = 0);
  virtual ~StSvtHybridPixelsC();

  StSvtHybridPixelsC& operator = (StSvtHybridPixelsC&);
  StSvtHybridPixelsC& operator + (StSvtHybridPixelsC&);

  unsigned char getPixelContent(int anode, int time);
  void addToPixel(int anode, int time, unsigned char x);
  void addToPixel(int index, unsigned char x);
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

  ClassDef(StSvtHybridPixelsC,1)
};

#endif
