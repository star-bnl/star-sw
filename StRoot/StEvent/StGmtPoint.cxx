/***************************************************************************
 *
 * Authors: K.S. Engle and Richard Witt (witt@usna.edu), Jan 2013
 * based on StFgtHit
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************/

#include "StGmtHit.h"
#include "StGmtPoint.h"
#include "StRoot/St_base/StMessMgr.h"

// constructor
StGmtPoint::StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key ) : StHit(), mKey( key ) {

   if( !hit1 || !hit2 ){
      LOG_ERROR << "Passed null pointer into StGmtPoint::StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key )" << endm;
      mKey = -999;
      return;
   }

   mHitLocalX = mHitLocalY = 0;
//    if( !(hit1->isY()) && hit2->isY() ){
//       mHitLocalX = hit1;
//       mHitLocalY = hit2;
//    } 
//    else if ( !(hit2->isY()) && hit1->isY() ){
//       mHitLocalX = hit2;
//       mHitLocalY = hit1;
//    }

   if( !mHitLocalX || !mHitLocalY ){
      LOG_ERROR << "Constructor not provided a (LocalX,LocalY) pair." << endm;
      mKey = -999;
      return;
   }

   if( mHitLocalX->getModule() != mHitLocalY->getModule() ){
      LOG_ERROR << "Cluster pair are not on the same Module." << endm;
      mKey = -999;
      return;
   }

   mHardwarePosition = mHitLocalX->hardwarePosition();
   mCharge = hit1->charge() + hit2->charge();
}

// deconstructor
StGmtPoint::~StGmtPoint() {
   // nothing to do
}

float StGmtPoint::getPositionLocalX() const { 
    return mHitLocalX->getLocalX();
}

float StGmtPoint::getPositionLocalY() const { 
    return mHitLocalY->getLocalY();
}

ClassImp(StGmtPoint)
