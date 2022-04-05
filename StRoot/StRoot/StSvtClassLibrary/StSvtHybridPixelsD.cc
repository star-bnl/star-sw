 /***************************************************************************
 *
 * $Id: StSvtHybridPixelsD.cc,v 1.5 2009/11/10 21:00:17 fisyak Exp $
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
#include "TMath.h"

ClassImp(StSvtHybridPixelsD)

//_____________________________________________________________________________
StSvtHybridPixelsD::StSvtHybridPixelsD() : 
  StSvtHybridObject(), TArrayD()
{
  // Default Constructor

  mNumberOfAnodes = 0;
  mNumberOfTimeBins = 0;

  mTotalNumberOfPixels = 0;

  mPedOffset = 0;
}

//_____________________________________________________________________________
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
      Set(mTotalNumberOfPixels,x);
    else
      Set(size);
  else
    Set(mTotalNumberOfPixels);
  mTrackId.Set(GetSize());
  mTrackId.Reset(0);

  mTruthTmp=0;
}

//_____________________________________________________________________________
StSvtHybridPixelsD::~StSvtHybridPixelsD()
{delete mTruthTmp;}

StSvtHybridPixelsD& StSvtHybridPixelsD::operator = (StSvtHybridPixelsD& h)
{
  double x;

  for (int i=0;i<mTotalNumberOfPixels;i++) {
    x = h.At(i);
    AddAt((double)x,i);
  }
  return *this;
}

//_____________________________________________________________________________
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

//_____________________________________________________________________________
double StSvtHybridPixelsD::getPixelContent(int anode, int time)
{
  // Returns the pixel content based on the anode and time bin numbers
  int index = getPixelIndex(anode, time);

  return (double)At(index);
}

//_____________________________________________________________________________
void StSvtHybridPixelsD::addToPixel(int anode, int time, double x,int trackId)
{
  int index = getPixelIndex(anode, time);
  addToPixel(index, x,trackId);
}

//_____________________________________________________________________________
void StSvtHybridPixelsD::addToPixel(int index, double x, int trackId)
{
  double x1, sum;

  x1 = At(index);
  sum = x1 + x;
  AddAt(sum,index);

  if (TMath::Abs(x)<=0) return;
  if (!mTruthTmp) mTruthTmp = new StMCPivotTruthMap;
  mTruthTmp->Add(index,trackId,TMath::Abs(x));
}

//_____________________________________________________________________________
int StSvtHybridPixelsD::getPixelIndex(int anode, int time)
{
  // Returns an internal index for pixel (anode,time). 
  // It should be used to store or retrieve a pixel value from this object.

  int index;

  index = mNumberOfTimeBins*(anode-1) + time;

  return index;
}

//_____________________________________________________________________________
void StSvtHybridPixelsD::reset()
{
  for (int i=0;i<mTotalNumberOfPixels;i++)
    AddAt(mPedOffset,i);  
}

//_____________________________________________________________________________
StMCTruth  StSvtHybridPixelsD::getTrackId(int index)
{
  return mTrackId[index];
}
//_____________________________________________________________________________
void StSvtHybridPixelsD::updateTruth()
{

  if (!mTruthTmp) return;
#if ROOT_VERSION_CODE <= ROOT_VERSION(5,22,0)
  Long_t index=-1;
#else
  Long64_t index=-1;
#endif
  while(1) {
    StMCTruth tru = mTruthTmp->Iter(index);
    if (index==-1) break;
    mTrackId.AddAt(tru,(int)index);
  }
  delete  mTruthTmp; mTruthTmp=0;
}  
  
  




