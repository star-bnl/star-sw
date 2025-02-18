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
#include "St_base/StMessMgr.h"

//________________
StGmtPoint::StGmtPoint() : StHit(), mHitLocalX(nullptr), mHitLocalY(nullptr), mKey(-999) { 
  /* empty */
}

//________________
StGmtPoint::StGmtPoint(StGmtHit* mHitLocalX, StGmtHit* mHitLocalY, int key) : StHit(), mKey( key ) {

    if ( !mHitLocalX || !mHitLocalX ) {
        LOG_ERROR << "Passed null pointer into StGmtPoint::StGmtPoint( StGmtHit* hit1, StGmtHit* hit2, int key )" << endm;
        LOG_ERROR << ( (!mHitLocalX && !mHitLocalY) ? "Both mHitLocalX and mHitLocalY are null." : 
                       (!mHitLocalX ? "mHitLocalX is null." : "mHitLocalY is null.")) << endm;
        mKey = -999;
    }
    else {
        // Check if both hits are from the same module
        if ( mHitLocalX->getModule() != mHitLocalX->getModule() ) {
            LOG_ERROR << "Cluster pair is not from the same module." << endm;
            mKey = -999;
        }

        mHardwarePosition = mHitLocalX->hardwarePosition();
        mCharge = mHitLocalX->charge() + mHitLocalX->charge();
    }
}

//________________
StGmtPoint::StGmtPoint(const StGmtPoint& p) : StHit(p), mKey(p.mKey), 
    mHitLocalX(p.mHitLocalX), mHitLocalY(p.mHitLocalY) {
    /* empty */
}

//________________
StGmtPoint::~StGmtPoint() {
    if ( mHitLocalX ) delete mHitLocalX;
    if ( mHitLocalY ) delete mHitLocalY;
}


