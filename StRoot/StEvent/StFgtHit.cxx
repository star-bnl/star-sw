/***************************************************************************
 *
 * $Id: StFgtHit.cxx,v 2.1 2012/04/16 20:20:49 ullrich Exp $
 * Author: S. Gliske, Oct 2011
 *
 ***************************************************************************
 *
 * Description: see header file.
 *
 ***************************************************************************
 *
 * $Log: StFgtHit.cxx,v $
 * Revision 2.1  2012/04/16 20:20:49  ullrich
 * Initial Revision
 *
 *
 **************************************************************************/

#include "StFgtHit.h"
#include <cmath>

// constructor
StFgtHit::StFgtHit( int key, int centralStripGeoId, float charge, 
                    short disc, short quad, char layer,
                    float rPos, float rErr, float phiPos, float phiErr, float zPos, float zErr )
{
    StHit::setHardwarePosition(disc*8+quad*2+(layer=='R'));
    StHit::setCharge(charge);
    mKey = key;    
    mR = rPos; 
    mErrR = rErr;
    mPhi = phiPos;
    mErrPhi =phiErr ;
    mCentralStripGeoId = centralStripGeoId;
    mChargeUncert = 1000;
    
   // Assert is being commented out.  If an invalid quad is specified, then
   // the hardwardware ID in the parent StHit will be incorrect.
   // assert( quad >= 0 && quad < 4 );
    
   mPosition.setX( rPos*cos(phiPos) );
   mPosition.setY( rPos*sin(phiPos) );
   mPosition.setZ( zPos );
    
   update2error();
   mPositionError.setZ( zErr );
}

// deconstructor
StFgtHit::~StFgtHit() {
    // nothing to do
}

// propagate the uncertainty on phi and r to uncertainty on x and y
void StFgtHit::update2error(){
    float cosPhiSq = cos( mPhi );
    cosPhiSq *= cosPhiSq;
    float sinPhiSq = 1 - cosPhiSq;
    
    float mErrRSq = mErrR*mErrR;
    float mErrPhiSq = mErrPhi*mErrPhi;
    
    float xErrSq = mErrRSq*cosPhiSq + mErrPhiSq*sinPhiSq;
    float yErrSq = mErrRSq*sinPhiSq + mErrPhiSq*cosPhiSq;
    
    mPositionError.setX( sqrt( xErrSq ) );
    mPositionError.setY( sqrt( yErrSq ) );
}

ClassImp(StFgtHit);
