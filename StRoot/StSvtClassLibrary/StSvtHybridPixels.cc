 /***************************************************************************
 *
 * $Id: StSvtHybridPixels.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixels class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixels.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is an array of 240 X 128 floats.                                    // 
// It can be used to store any pixel data from one hybrid.                //
// For instance, the pedestals can be defined as a StSvtHybridPixels.     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <iostream.h>
#include "StSvtHybridPixels.hh"

ClassImp(StSvtHybridPixels)

StSvtHybridPixels::StSvtHybridPixels() : 
  StSvtHybridObject(), TArrayF()
{
  // Default Constructor

  mNumberOfAnodes = 0;
  mNumberOfTimeBins = 0;

  mTotalNumberOfPixels = 0;
}

StSvtHybridPixels::StSvtHybridPixels(int barrel, int ladder, int wafer, int hybrid, int size, float* x) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid), TArrayF(size, x)
{
  // The same as StSvtHybridObject.

  mNumberOfAnodes = 240;
  mNumberOfTimeBins = 128;

  mTotalNumberOfPixels = mNumberOfAnodes*mNumberOfTimeBins;

  if (!size)
    Set(mTotalNumberOfPixels);
}

StSvtHybridPixels::~StSvtHybridPixels()
{}

StSvtHybridPixels& StSvtHybridPixels::operator = (StSvtHybridPixels& h)
{
  float x;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x = h.At(i);
    AddAt(x,i);
  }
  return *this;
}

StSvtHybridPixels& StSvtHybridPixels::operator + (StSvtHybridPixels& h)
{
  float x1, x2;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x1 = At(i);
    x2 = h.At(i);
    AddAt(x1+x2,i);
  }
  return *this;
}

Float_t StSvtHybridPixels::getPixelContent(int anode, int time)
{
  // Returns the pixel content based on the anode and time bin numbers
  int index = getPixelIndex(anode, time);

  return At(index);
}

int StSvtHybridPixels::getPixelIndex(int anode, int time)
{
  // Returns an internal index for pixel (anode,time). 
  // It should be used to store or retrieve a pixel value from this object.

  int index;

  index = mNumberOfTimeBins*(anode-1) + time;

  return index;
}

