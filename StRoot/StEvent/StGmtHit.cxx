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
#include <cmath>

// constructor
StGmtHit::StGmtHit( int key, short module, int adcX, int adcY,
	      int tbX, int tbY, double localX, double localY,
	      double localXErr, double localYErr)
{
    StHit::setHardwarePosition(module);
    StHit::setCharge(0);
    mKey = key;    
    mModule = module;    
    mAdcX = adcX;    
    mAdcY = adcY;    
    mTbX = tbX;    
    mTbY = tbY;    
    mLocalX = localX; 
    mLocalY = localY; 
    mErrLocalX = localXErr;
    mErrLocalY = localYErr;
    mChargeUncert = 1000;
        
    mPosition.setX( localX ); 
    mPosition.setY( localY );
//     mPosition.setZ( zPos );  //////// FIX ME!!!!!!!!!!
    
   update2error();
//    mPositionError.setZ( zErr );  //////// FIX ME!!!!!!!!!!
}

// deconstructor
StGmtHit::~StGmtHit() {
    // nothing to do
}

// propagate the uncertainty on phi and r to uncertainty on x and y
void StGmtHit::update2error(){
//     float cosPhiSq = cos( mPhi );
//     cosPhiSq *= cosPhiSq;
//     float sinPhiSq = 1 - cosPhiSq;
//     
//     float mErrRSq = mErrR*mErrR;
//     float mErrPhiSq = mErrPhi*mErrPhi;
//     
//     float xErrSq = mErrRSq*cosPhiSq + mErrPhiSq*sinPhiSq;
//     float yErrSq = mErrRSq*sinPhiSq + mErrPhiSq*cosPhiSq;
//     
//     mPositionError.setX( sqrt( xErrSq ) );
//     mPositionError.setY( sqrt( yErrSq ) );
    
    mPositionError.setX( mErrLocalX );  //////// FIX ME!!!!!!!!!!
    mPositionError.setY( mErrLocalY );  //////// FIX ME!!!!!!!!!!

}

ClassImp(StGmtHit)
