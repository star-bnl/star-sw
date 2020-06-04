/***************************************************************************
 *
 * $Id: StETofPidTraits.cxx,v 2.1 2019/02/11 18:41:19 ullrich Exp $
 *
 * Author: Florian Seck, August 2018
 ***************************************************************************
 *
 * Description: Data class for storing eTOF PID information for tracks
 * matched to eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StETofPidTraits.cxx,v $
 * Revision 2.1  2019/02/11 18:41:19  ullrich
 * Initial Revision
 *
 *
 ***************************************************************************/ 
#include "StETofPidTraits.h"

StETofPidTraits::StETofPidTraits()
: StTrackPidTraits( kETofId )
{
  mETofHit      = nullptr;

  mMatchFlag    = 0;
  mLocalX       = -999.;
  mLocalY       = -999.;
  mThetaLocal   = -999.;
  mDeltaX       = -999.;
  mDeltaY       = -999.;
  mPosition.set( 0., 0., 0. );
  mTimeOfFlight = -999.;
  mPathLength   = -999.;
  mBeta         = -999.;

}

StETofPidTraits::~StETofPidTraits()
{
    /* no op */
}

StETofHit*
StETofPidTraits::etofHit()
{
    return mETofHit;
}

const StETofHit*
StETofPidTraits::etofHit() const
{
    return mETofHit;
}


StThreeVectorF&
StETofPidTraits::position()
{
    return mPosition;
}

const StThreeVectorF&
StETofPidTraits::position() const
{
    return mPosition;
}

void
StETofPidTraits::setETofHit( StETofHit* hit )
{
    mETofHit = hit;
}

void
StETofPidTraits::setPosition( const StThreeVectorF& pos )
{
    mPosition = pos;
}
