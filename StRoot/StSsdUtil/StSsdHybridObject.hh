/***************************************************************************
 *
 * $Id: StSsdHybridObject.hh,v 1.1 2004/03/12 04:24:20 jeromel Exp $
 *
 * Author: cr
 ***************************************************************************
 *
 * Description: SVT Hybrid BASE class
 *
 **************************************************************************/

#ifndef STSSDHYBRIDOBJECT_HH
#define STSSDHYBRIDOBJECT_HH

#include "StObject.h"

class StSsdHybridObject: public StObject
{
public:
  StSsdHybridObject();
  StSsdHybridObject(int barrel, int ladder, int wafer, int hybrid);
  virtual ~StSsdHybridObject();
  StSsdHybridObject(const StSsdHybridObject&);
  StSsdHybridObject& operator = (const StSsdHybridObject&);

  int getBarrelID() {return mBarrel;} /// return Barrel number
  int getLadderID() {return mLadder;} /// return Ladder number
  int getWaferID()  {return mWafer;}  /// return Wafer number
  int getHybridID() {return mHybrid;} /// return Hybrid number
  int getLayerID();                   /// return Layer number

  void setBarrelID(int barrel) {mBarrel = barrel;} /// set Barrel number
  void setLadderID(int ladder) {mLadder = ladder;} /// set Ladder number
  void setWaferID(int wafer)   {mWafer = wafer;}   /// set Wafer number
  void setHybridID(int hybrid) {mHybrid = hybrid;} /// set Hybrid number
  void setHybrid(int barrel, int ladder, int wafer, int hybrid) {mBarrel = barrel;
                                                                 mLadder = ladder;
                                                                 mWafer = wafer;
                                                                 mHybrid = hybrid;}

protected:

  int mBarrel; ///  Barrel number
  int mLayer;  ///  Layer number
  int mLadder; ///  Ladder number
  int mWafer;  ///  Wafer number
  int mHybrid; ///  Hybrid number

  ClassDef(StSsdHybridObject,1)
};

#endif
