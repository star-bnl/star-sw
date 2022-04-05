 /***************************************************************************
 *
 * $Id: StSvtHybridPixels.cc,v 1.4 2003/09/02 17:59:06 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Pixels class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixels.cc,v $
 * Revision 1.4  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.3  2000/11/30 20:39:12  caines
 * Changed to allow us of database
 *
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

#include <Stiostream.h>
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
  StSvtHybridObject(barrel, ladder, wafer, hybrid)
{
  // The same as StSvtHybridObject.

  mNumberOfAnodes = 240;
  mNumberOfTimeBins = 128;

  mTotalNumberOfPixels = mNumberOfAnodes*mNumberOfTimeBins;

  if (size)
    if (x)
      Set(mTotalNumberOfPixels,x);
    else
      Set(size);
  else
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

void StSvtHybridPixels::addToPixel(int anode, int time, float x)
{
  float x1;
  int index = getPixelIndex(anode, time);

   x1 = At(index);
   AddAt(x+x1,index);
}

void StSvtHybridPixels::addToPixel(int index, float x)
{
  float x1;

   x1 = At(index);
   AddAt(x+x1,index);
}

int StSvtHybridPixels::getPixelIndex(int anode, int time)
{
  // Returns an internal index for pixel (anode,time). 
  // It should be used to store or retrieve a pixel value from this object.

  int index;

  index = mNumberOfTimeBins*(anode-1) + time;

  return index;
}

void StSvtHybridPixels::reset()
{
  for (int i=0;i<mTotalNumberOfPixels;i++)
    AddAt(0,i);  
}
