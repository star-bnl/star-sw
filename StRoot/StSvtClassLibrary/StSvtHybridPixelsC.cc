 /***************************************************************************
 *
 * $Id: StSvtHybridPixelsC.cc,v 1.3 2003/09/02 17:59:06 perev Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid PixelsC class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridPixelsC.cc,v $
 * Revision 1.3  2003/09/02 17:59:06  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.2  2002/02/12 23:09:50  munhoz
 * fixing problems for new compiler
 *
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
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// It is an array of 240 X 128 floats.                                    // 
// It can be used to store any pixel data from one hybrid.                //
// For instance, the pedestals can be defined as a StSvtHybridPixelsC.     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include <Stiostream.h>
#include "StSvtHybridPixelsC.hh"

ClassImp(StSvtHybridPixelsC)

StSvtHybridPixelsC::StSvtHybridPixelsC() : 
  StSvtHybridObject(), TArrayC()
{
  // Default Constructor

  mNumberOfAnodes = 0;
  mNumberOfTimeBins = 0;

  mTotalNumberOfPixels = 0;

  mPedOffset = 0;
}

StSvtHybridPixelsC::StSvtHybridPixelsC(int barrel, int ladder, int wafer, int hybrid, int size, unsigned char* x) : 
  StSvtHybridObject(barrel, ladder, wafer, hybrid), TArrayC()
{
  // The same as StSvtHybridObject.

  mNumberOfAnodes = 240;
  mNumberOfTimeBins = 128;

  mTotalNumberOfPixels = mNumberOfAnodes*mNumberOfTimeBins;

  mPedOffset = 0;

  if (size)
    if (x)
      Set(mTotalNumberOfPixels,(char*)x);
    else
      Set(size);
  else
    Set(mTotalNumberOfPixels);
}

StSvtHybridPixelsC::~StSvtHybridPixelsC()
{}

StSvtHybridPixelsC& StSvtHybridPixelsC::operator = (StSvtHybridPixelsC& h)
{
  float x;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x = h.At(i);
    AddAt((char)x,i);
  }
  return *this;
}

StSvtHybridPixelsC& StSvtHybridPixelsC::operator + (StSvtHybridPixelsC& h)
{
  float x1, x2;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x1 = (float)At(i);
    x2 = (float)h.At(i);

    if ((x1+x2) < 255)
      AddAt((char)(x1+x2),i);
    else
      AddAt(255,i);      
  }
  return *this;
}

unsigned char StSvtHybridPixelsC::getPixelContent(int anode, int time)
{
  // Returns the pixel content based on the anode and time bin numbers
  int index = getPixelIndex(anode, time);

  return (unsigned char)At(index);
}

void StSvtHybridPixelsC::addToPixel(int anode, int time, unsigned char x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsC::addToPixel(int index, unsigned char x)
{
  int x1, sum;

   x1 = (int)At(index);

   sum = x1 + (int)x;

   if (sum < 255)
     AddAt((unsigned char)sum,index);
   else
     AddAt((unsigned char)255,index);
}

void StSvtHybridPixelsC::addToPixel(int anode, int time, int x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsC::addToPixel(int index, int x)
{
  int x1, sum;

  x1 = (int)At(index);
  
  sum = x1 + x;
  
  if (sum >=0 && sum < 255)
    AddAt((unsigned char)sum,index);
  else if (sum < 0)
    AddAt((unsigned char)0,index);
  else if (sum > 255)
    AddAt((unsigned char)255,index);
}

void StSvtHybridPixelsC::addToPixel(int anode, int time, double x)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x);
}

void StSvtHybridPixelsC::addToPixel(int index, double x)
{
  double x1, sum;

  x1 = (double)At(index);
  
  sum = x1 + x;
  
  if (sum >=0 && sum < 255)
    AddAt((unsigned char)sum,index);
  else if (sum < 0)
    AddAt((unsigned char)0,index);
  else if (sum > 255)
    AddAt((unsigned char)255,index);
}

int StSvtHybridPixelsC::getPixelIndex(int anode, int time)
{
  // Returns an internal index for pixel (anode,time). 
  // It should be used to store or retrieve a pixel value from this object.

  int index;

  index = mNumberOfTimeBins*(anode-1) + time;

  return index;
}

void StSvtHybridPixelsC::reset()
{
  for (int i=0;i<mTotalNumberOfPixels;i++)
    AddAt(0,i);  
}
