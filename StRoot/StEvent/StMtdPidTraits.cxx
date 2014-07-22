/***************************************************************************
 *
 * $Id: StMtdPidTraits.cxx,v 2.3 2014/07/22 01:45:13 ullrich Exp $
 *
 * Author: Frank Geurts (Rice)
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdPidTraits.cxx,v $
 * Revision 2.3  2014/07/22 01:45:13  ullrich
 * Added residuals (dz,dy) between matched track-hit pairs nd access functions (Rongrong Ma)
 *
 * Revision 2.2  2014/05/29 16:58:05  ullrich
 * Added new member mExpTimeOfFlight and referring access methods.
 *
 * Revision 1.2  2014/05/22 19:04:25  marr
 * locally backup /star/u/marr/data02/mtd/dev/StRoot/StEvent/StMtdPidTraits.cxx
 *
 * Revision 2.1  2013/02/21 00:23:09  ullrich
 * Initial Revision.
 *
 ***************************************************************************/
#include "StMtdPidTraits.h"

static const char rcsid[] = "$Id: StMtdPidTraits.cxx,v 2.3 2014/07/22 01:45:13 ullrich Exp $";

ClassImp(StMtdPidTraits)

StMtdPidTraits::StMtdPidTraits() : StTrackPidTraits(kMtdId)
{
      mMtdHit = 0;

      mMatchFlag         = 0;
      mYLocal            = -999.;
      mZLocal            = -999.;
      mThetaLocal        = -999.;
      mTimeOfFlight      = -999.;
      mPathLength        = -999.;
      mBeta              = -999.;

      mSigmaMuon         = -999.; 
      mProbMuon          = -999.; 
      mExpTimeOfFlight   = -999.;
      mDeltaY            = -999.;
      mDeltaZ            = -999.;
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
