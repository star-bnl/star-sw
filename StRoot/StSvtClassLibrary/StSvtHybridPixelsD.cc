 /***************************************************************************
 *
 * $Id: StSvtHybridPixelsD.cc,v 1.2 2003/09/07 03:49:05 perev Exp $
 *
 * Author: Petr Chaloupka
 ***************************************************************************
 *
 * Description: SVT Hybrid PixelsD class
 *
 ***************************************************************************
 */

////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is an array of 240 X 128 doubles.                                   // 
// It can be used to store any pixel data from one hybrid.                //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "Stiostream.h"
#include "StSvtHybridPixelsD.hh"


ClassImp(StSvtHybridPixelsD)

StSvtHybridPixelsD::StSvtHybridPixelsD() : 
  StSvtHybridObject(), TArrayD()
{
  // Default Constructor

  mNumberOfAnodes = 0;
  mNumberOfTimeBins = 0;

  mTotalNumberOfPixels = 0;

  mPedOffset = 0;
}

StSvtHybridPixelsD::StSvtHybridPixelsD(int barrel, int ladder, int wafer, int hybrid, int size, double* x) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid), TArrayD()
{
  // The same as StSvtHybridObject.

  mNumberOfAnodes = 240;
  mNumberOfTimeBins = 128;

  mTotalNumberOfPixels = mNumberOfAnodes*mNumberOfTimeBins;

  mPedOffset = 0;

  if (size)
    if (x)
      Set(mTotalNumberOfPixels,(double*)x);
    else
      Set(size);
  else
    Set(mTotalNumberOfPixels);
}

StSvtHybridPixelsD::~StSvtHybridPixelsD()
{}

StSvtHybridPixelsD& StSvtHybridPixelsD::operator = (StSvtHybridPixelsD& h)
{
  double x;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x = h.At(i);
    AddAt((double)x,i);
  }
  return *this;
}

StSvtHybridPixelsD& StSvtHybridPixelsD::operator + (StSvtHybridPixelsD& h)
{
  double x1, x2;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x1 = (double)At(i);
    x2 = (double)h.At(i);

    if ((x1+x2) < 255)
      AddAt((double)(x1+x2),i);
    else
      AddAt(255,i);      
  }
  return *this;
}

double StSvtHybridPixelsD::getPixelContent(int anode, int time)
{
  // Returns the pixel content based on the anode and time bin numbers
  int index = getPixelIndex(anode, time);

  return (double)At(index);
}

void StSvtHybridPixelsD::addToPixel(int anode, int time, char x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsD::addToPixel(int index,  char x)
{
  double x1, sum;

   x1 = At(index);

   sum = x1 + x;

   if (sum < 255)
     AddAt(sum,index);
   else
     AddAt(255,index);
}

void StSvtHybridPixelsD::addToPixel(int anode, int time, int x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsD::addToPixel(int index, int x)
{
  double x1, sum;

  x1 = At(index);
  
  sum = x1 + x;
  
  if (sum >=0 && sum < 255)
    AddAt(sum,index);
  else if (sum < 0)
    AddAt(0,index);
  else if (sum > 255)
    AddAt(255,index);
}

void StSvtHybridPixelsD::addToPixel(int anode, int time, double x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsD::addToPixel(int index, double x)
{
  double x1, sum;

  x1 = At(index);
  
  sum = x1 + x;
  
  if (sum >=0 && sum < 255)
    AddAt(sum,index);
  else if (sum < 0)
    AddAt(0,index);
  else if (sum > 255)
    AddAt(255,index);
}

int StSvtHybridPixelsD::getPixelIndex(int anode, int time)
{
  // Returns an internal index for pixel (anode,time). 
  // It should be used to store or retrieve a pixel value from this object.

  int index;

  index = mNumberOfTimeBins*(anode-1) + time;

  return index;
}

void StSvtHybridPixelsD::reset()
{
  for (int i=0;i<mTotalNumberOfPixels;i++)
    AddAt(mPedOffset,i);  
}
