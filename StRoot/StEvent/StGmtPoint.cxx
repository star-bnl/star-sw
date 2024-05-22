/**
 * \class StGmtPoint
 * \brief Holds data for the point (a.k.a. cluster) in GMT
 * 
 * Description: data for individual ``point'' on the GMT, i.e. a pair
 * of 1D clusters.  Note, if errors during construction, the key will
 * be set to -999. Based on StFgtHit.
 *
 * \author K.S. Engle, Jan. 2013
 * \author Richard Witt (witt@usna.edu), Jan. 2013
 * \author Grigory Nigmatkulov (nigmatkulov@gmail.com), Dec. 2020
 */

// STAR headers
#include "StGmtHit.h"
#include "StGmtPoint.h"
#include "StRoot/St_base/StMessMgr.h"

ClassImp(StGmtPoint)

//________________
StGmtPoint::StGmtPoint() : StHit(), mHitLocalX(0), mHitLocalY(0) { 
  /* empty */
};

//________________
StGmtPoint::StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key ) : StHit(), mKey( key ) {
   if ( !hit1 || !hit2 ) {
     LOG_ERROR << "Passed null pointer into StGmtPoint::StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key )" << endm;
     mKey = -999;
     return;
   }

   mHitLocalX = mHitLocalY = 0;

   if ( !mHitLocalX || !mHitLocalY ) {
     LOG_ERROR << "Constructor not provided a (LocalX,LocalY) pair." << endm;
     mKey = -999;
     return;
   }

   if ( mHitLocalX->getModule() != mHitLocalY->getModule() ) {
     LOG_ERROR << "Cluster pair are not on the same Module." << endm;
     mKey = -999;
     return;
   }

   mHardwarePosition = mHitLocalX->hardwarePosition();
   mCharge = hit1->charge() + hit2->charge();
}

//________________
StGmtPoint::~StGmtPoint() {
 /* empty */
}


