/***************************************************************************
 *
 * $Id: StSsdHybridObject.cc,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SSD Hybrid Object BASE class
 *                                                                        
 * This class represents the basic unit of the SSD, i.e, a hybrid.        
 * It should be the base class for any "hybrid object".                   
 * "Anything" (data, pedestal, clusters, hit points, etc.)
 * that is particular to one hybrid, should inherit from this class.      
 *
 ***************************************************************************/


#include "StSsdHybridObject.hh"
#include "StMessMgr.h"

ClassImp(StSsdHybridObject)

///Default constructor.
StSsdHybridObject::StSsdHybridObject()
{
  mBarrel = 0;
  mLadder = 0;
  mWafer  = 0;
  mHybrid = 0;
}

/// This constructor has four input parameters: Barrel, Ladder, Wafer and Hybrid number (as expected).
StSsdHybridObject::StSsdHybridObject(int barrel, int ladder, int wafer, int hybrid)
{
  mBarrel = barrel;
  mLadder = ladder;
  mWafer  = wafer;
  mHybrid = hybrid;
}

StSsdHybridObject::~StSsdHybridObject()
{}

StSsdHybridObject::StSsdHybridObject(const StSsdHybridObject& hybrid)
{
  // Copy Constructor
  mBarrel = hybrid.mBarrel;
  mLadder = hybrid.mLadder;
  mWafer  = hybrid.mWafer;
  mHybrid = hybrid.mHybrid;
}

StSsdHybridObject& StSsdHybridObject::operator = (const StSsdHybridObject& hybrid)
{
  mBarrel = hybrid.mBarrel;
  mLadder = hybrid.mLadder;
  mWafer  = hybrid.mWafer;
  mHybrid = hybrid.mHybrid;
  return *this;
}

int StSsdHybridObject::getLayerID()
{
  switch (mBarrel) {
  case 1:
    mLayer = 1;
    break;

  default:
    gMessMgr->Error() << "StSsdHybridObject::getLayerID : untreated layer value " << mLayer << endl;
  }

  return mLayer;
}

