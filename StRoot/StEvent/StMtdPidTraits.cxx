/***************************************************************************
 *
 * $Id: StMtdPidTraits.cxx,v 2.1 2013/02/21 00:23:09 ullrich Exp $
 *
 * Author: Frank Geurts (Rice)
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdPidTraits.cxx,v $
 * Revision 2.1  2013/02/21 00:23:09  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "StMtdPidTraits.h"

static const char rcsid[] = "$Id: StMtdPidTraits.cxx,v 2.1 2013/02/21 00:23:09 ullrich Exp $";

ClassImp(StMtdPidTraits)

StMtdPidTraits::StMtdPidTraits() : StTrackPidTraits(kMtdId)
{
      mMtdHit = 0;

      mMatchFlag     = 0;
      mYLocal        = -999.;
      mZLocal        = -999.;
      mThetaLocal    = -999.;
      mTimeOfFlight  = -999.;
      mPathLength    = -999.;
      mBeta          = -999.;

      mSigmaMuon     = -999.; 
      mProbMuon      = -999.; 
}

StMtdPidTraits::~StMtdPidTraits() { /* noop */ }

StMtdHit*
StMtdPidTraits::mtdHit() { return mMtdHit; }

const StMtdHit*
StMtdPidTraits::mtdHit() const { return mMtdHit; }

StThreeVectorF&
StMtdPidTraits::position() { return mPosition; }

const StThreeVectorF&
StMtdPidTraits::position() const { return mPosition; }

void
StMtdPidTraits::setMtdHit(StMtdHit* hit) { mMtdHit = hit; }

void
StMtdPidTraits::setPosition(const StThreeVectorF& pos) { mPosition = pos; }
