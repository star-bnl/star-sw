/***************************************************************************
 *
 * $Id: StBTofPidTraits.cxx,v 2.2 2009/12/08 23:24:46 fine Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBTofPidTraits.cxx,v $
 * Revision 2.2  2009/12/08 23:24:46  fine
 * Fix issue  #1748 http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1748
 *
 * Revision 2.1  2008/12/22 20:30:59  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StBTofPidTraits.h"

static const char rcsid[] = "$Id: StBTofPidTraits.cxx,v 2.2 2009/12/08 23:24:46 fine Exp $";

ClassImp(StBTofPidTraits)

StBTofPidTraits::StBTofPidTraits() : StTrackPidTraits(kTofId)
{
      mBTofHit = 0;

      mMatchFlag     = 0;
      mYLocal        = -999.;
      mZLocal        = -999.;
      mThetaLocal    = -999.;
      mTimeOfFlight  = -999.;
      mPathLength    = -999.;
      mBeta          = -999.;

      mSigmaElectron = -999.; 
      mSigmaPion     = -999.; 
      mSigmaKaon     = -999.; 
      mSigmaProton   = -999.;  
      mProbElectron  = -999.; 
      mProbPion      = -999.; 
      mProbKaon      = -999.; 
      mProbProton    = -999.;  
}

StBTofPidTraits::~StBTofPidTraits() { /* noop */ }

StBTofHit*
StBTofPidTraits::tofHit() { return mBTofHit; }

const StBTofHit*
StBTofPidTraits::tofHit() const { return mBTofHit; }

StThreeVectorF&
StBTofPidTraits::position() { return mPosition; }

const StThreeVectorF&
StBTofPidTraits::position() const { return mPosition; }

float
StBTofPidTraits::tot() const
{
    if(mBTofHit) return mBTofHit->tot();
    else return -999.;
}

void
StBTofPidTraits::setTofHit(StBTofHit* hit) { mBTofHit = hit; }

void
StBTofPidTraits::setPosition(const StThreeVectorF& pos) { mPosition = pos; }
