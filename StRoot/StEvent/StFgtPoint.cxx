/***************************************************************************
 *
 * $Id: StFgtPoint.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtPoint.cxx,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StFgtHit.h"
#include "StFgtPoint.h"
#include "StRoot/St_base/StMessMgr.h"

// constructor
StFgtPoint::StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key ) : StHit(), mKey( key ) {

   if( !hit1 || !hit2 ){
      LOG_ERROR << "Passed null pointer into StFgtPoint::StFgtPoint( StFgtHit* hit1, StFgtHit* hit2, int key )" << endm;
      mKey = -999;
      return;
   }

   mHitR = mHitPhi = 0;
   if( hit1->getLayer() == 'R' && hit2->getLayer() == 'P' ){
      mHitR = hit1;
      mHitPhi = hit2;
   } 
   else if ( hit2->getLayer() == 'R' && hit1->getLayer() == 'P' ){
      mHitR = hit2;
      mHitPhi = hit1;
   }

   if( !mHitR || !mHitPhi ){
      LOG_ERROR << "Constructor not provided a r/phi pair." << endm;
      mKey = -999;
      return;
   }

   if( mHitR->getDisc() != mHitPhi->getDisc() ){
      LOG_ERROR << "Cluster pair are not on the same disc." << endm;
      mKey = -999;
      return;
   }

   if( mHitR->getQuad() != mHitPhi->getQuad() ){
      LOG_ERROR << "Cluster pair are not on the same quadrant." << endm;
      mKey = -999;
      return;
   }

   mHardwarePosition = mHitR->hardwarePosition();
   mCharge = hit1->charge() + hit2->charge();
}

// deconstructor
StFgtPoint::~StFgtPoint() {
   // nothing to do
}

float StFgtPoint::getPositionR() const {
    return mHitR->getPositionR();
}

float StFgtPoint::getPositionPhi() const {
    return mHitPhi->getPositionPhi();
}

ClassImp(StFgtPoint);
