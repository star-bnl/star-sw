/***************************************************************************
 *
 * $Id: StSvtHybridObject.cc,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Object BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridObject.cc,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This class represents the basic unit of the SVT, i.e, a hybrid.        //
// It should be the base class for any "hybrid object".                   //
// "Anything" (data, pedestal, drift velocity, clusters, hit points, etc.)// 
// that is particular to one hybrid, should inherit from this class.      // 
//                                                                        //
////////////////////////////////////////////////////////////////////////////


#include "StSvtHybridObject.hh"

ClassImp(StSvtHybridObject)

StSvtHybridObject::StSvtHybridObject()
{
  //Default constructor.
  mBarrel = 0;
  mLadder = 0;
  mWafer  = 0;
  mHybrid = 0;
}

StSvtHybridObject::StSvtHybridObject(int barrel, int ladder, int wafer, int hybrid)
{
  //This constructor has four input parameters: Barrel, Ladder, Wafer and Hybrid number (as expected).
  mBarrel = barrel;
  mLadder = ladder;
  mWafer  = wafer;
  mHybrid = hybrid;
}

StSvtHybridObject::~StSvtHybridObject()
{}

StSvtHybridObject::StSvtHybridObject(const StSvtHybridObject& hybrid)
{
  // Copy Constructor
  mBarrel = hybrid.mBarrel;
  mLadder = hybrid.mLadder;
  mWafer  = hybrid.mWafer;
  mHybrid = hybrid.mHybrid;
}

StSvtHybridObject& StSvtHybridObject::operator = (const StSvtHybridObject& hybrid)
{
  mBarrel = hybrid.mBarrel;
  mLadder = hybrid.mLadder;
  mWafer  = hybrid.mWafer;
  mHybrid = hybrid.mHybrid;
  return *this;
}

int StSvtHybridObject::getLayerID()
{
  switch (mBarrel) {

  case 1:
    if (mLadder%2)
      mLayer = 2;
    else
      mLayer = 1;

    break;

  case 2:    
    if (mLadder%2)
      mLayer = 3;
    else
      mLayer = 4;

    break;

  case 3:
    if (mLadder%2)
      mLayer = 6;
    else
      mLayer = 5;

    break;
  }

  return mLayer;
}

