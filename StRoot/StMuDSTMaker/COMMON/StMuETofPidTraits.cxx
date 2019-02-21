/***************************************************************************
 *
 * $Id: StMuETofPidTraits.cxx,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, November 2018
 ***************************************************************************
 *
 * Description: Data class for storing eTOF PID information for tracks
 * matched to eTOF hits
 *
 ***************************************************************************
 *
 * $Log: StMuETofPidTraits.cxx,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#include "StETofPidTraits.h"
#include "StMuETofPidTraits.h"

StMuETofPidTraits::StMuETofPidTraits()
{
    mMatchFlag    = 0;
    mLocalX       = -999.;
    mLocalY       = -999.;
    mThetaLocal   = -999.;
    mPosition.set( 0., 0., 0. );
    mDeltaX       = -999.;
    mDeltaY       = -999.;
    mTimeOfFlight = -999.;
    mPathLength   = -999.;
    mBeta         = -999.;
}

StMuETofPidTraits::~StMuETofPidTraits()
{ /* noop */
}

void
StMuETofPidTraits::setETofPidTraits( const StETofPidTraits* pid )
{
    if( !pid ) {
        mMatchFlag    = 0;
        mLocalX       = -999.;
        mLocalY       = -999.;
        mThetaLocal   = -999.;
        mPosition.set( 0., 0., 0. );
        mDeltaX       = -999.;
        mDeltaY       = -999.;
        mTimeOfFlight = -999.;
        mPathLength   = -999.;
        mBeta         = -999.;
    }
    else{   
        mMatchFlag     = pid->matchFlag();
        mLocalX        = pid->localX();
        mLocalY        = pid->localY();
        mThetaLocal    = pid->thetaLocal();
        mPosition      = pid->position();
        mDeltaX        = pid->deltaX();
        mDeltaY        = pid->deltaY();
        mTimeOfFlight  = pid->timeOfFlight();
        mPathLength    = pid->pathLength();
        mBeta          = pid->beta();
    }
}

StETofPidTraits*
StMuETofPidTraits::createETofPidTraits() const
{
    StETofPidTraits* traits = new StETofPidTraits();
    
    traits->setMatchFlag(    mMatchFlag    );
    traits->setLocalX(       mLocalX       );
    traits->setLocalY(       mLocalY       );
    traits->setThetaLocal(   mThetaLocal   );
    traits->setPosition(     mPosition     );
    traits->setDeltaX(       mDeltaX       );
    traits->setDeltaY(       mDeltaY       );
    traits->setTimeOfFlight( mTimeOfFlight );
    traits->setPathLength(   mPathLength   );
    traits->setBeta(         mBeta         );

    return traits;
}

StThreeVectorF&
StMuETofPidTraits::position() {
    return mPosition;
}

const StThreeVectorF&
StMuETofPidTraits::position() const {
    return mPosition;
}

void
StMuETofPidTraits::setPosition( const StThreeVectorF& pos ) {
    mPosition = pos;
}
