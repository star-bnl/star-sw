/***************************************************************************
 *
 * $Id: StSvtHybridObject.hh,v 1.1.1.1 2000/03/10 14:26:21 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridObject.hh,v $
 * Revision 1.1.1.1  2000/03/10 14:26:21  munhoz
 * SVT Class Library
 *
 **************************************************************************/

#ifndef STSVTHYBRIDOBJECT_HH
#define STSVTHYBRIDOBJECT_HH

#include "StObject.h"

class StSvtHybridObject: public StObject
{
public:
  StSvtHybridObject();
  StSvtHybridObject(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSvtHybridObject();
  StSvtHybridObject(const StSvtHybridObject&);
  StSvtHybridObject& operator = (const StSvtHybridObject&);

  int getBarrelID() {return mBarrel;} // return Barrel number
  int getLadderID() {return mLadder;} // return Ladder number
  int getWaferID()  {return mWafer;}  // return Wafer number
  int getHybridID() {return mHybrid;} // return Hybrid number
  int getLayerID();                   // return Layer number

  void setBarrelID(int barrel) {mBarrel = barrel;} // set Barrel number
  void setLadderID(int ladder) {mLadder = ladder;} // set Ladder number
  void setWaferID(int wafer)   {mWafer = wafer;}    // set Wafer number
  void setHybridID(int hybrid) {mHybrid = hybrid;} // set Hybrid number
  void setHybrid(int barrel, int ladder, int wafer, int hybrid) {mBarrel = barrel;
                                                                 mLadder = ladder;
                                                                 mWafer = wafer;
                                                                 mHybrid = hybrid;}

protected:

  int mBarrel; //  Barrel number
  int mLayer;  //  Layer number
  int mLadder; //  Ladder number
  int mWafer;  //  Wafer number
  int mHybrid; //  Hybrid number

  ClassDef(StSvtHybridObject,1)
};

#endif
