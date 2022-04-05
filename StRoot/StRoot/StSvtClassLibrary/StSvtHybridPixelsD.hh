/***************************************************************************
 *
 * $Id: StSvtHybridPixelsD.hh,v 1.2 2005/07/23 03:37:33 perev Exp $
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
#include "TArrayI.h"
#include "StMCTruth.h"
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
  void addToPixel(int anode, int time, double x,int trackId);
  void addToPixel(int index, double x,int trackId);

  int getNumberOfAnodes() 	{return mNumberOfAnodes;}
  int getNumberOfTimeBins() 	{return mNumberOfTimeBins;}
  int getTotalNumberOfPixels()  {return mTotalNumberOfPixels;}
  int getPixelIndex(int anode, int time);

  StMCTruth getTrackId(int index);

  void setPedOffset(int offset)	{mPedOffset = offset;}
  int  getPedOffset()		{return mPedOffset;}

  void reset();
  void updateTruth();
protected:
  TArrayI mTrackId;
  int mNumberOfAnodes;   // Number of Anodes in one hybrid (= 240)
  int mNumberOfTimeBins; // Number of Time Bins in one hybrid (= 128)
  
  int mTotalNumberOfPixels; // Total Number of Pixels (= 240*128)

  int mPedOffset;

  StMCPivotTruthMap *mTruthTmp;

  ClassDef(StSvtHybridPixelsD,1)
};

#endif
