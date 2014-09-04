#include "StMtdPidTraits.h"
#include "StMuMtdPidTraits.h"
#include "StMuMtdHit.h"

static const char rcsid[] = "$Id: StMuMtdPidTraits.cxx,v 1.3 2014/07/22 19:04:00 jdb Exp $";

ClassImp(StMuMtdPidTraits)

StMuMtdPidTraits::StMuMtdPidTraits() 
{
      mMatchFlag          = 0;
      mYLocal             = -999.;
      mZLocal             = -999.;
      mThetaLocal         = -999.;
      mTimeOfFlight       = -999.;
      mPathLength         = -999.;
      mBeta               = -999.;
      mPosition.set(0,0,0);

      mSigmaMuon          = -999.; 
      mProbMuon           = -999.; 
      mExpTimeOfFlight    = -999.;
      mDeltaY             = -999.;
      mDeltaZ             = -999.;
}

StMuMtdPidTraits::~StMuMtdPidTraits() { /* noop */ }


void StMuMtdPidTraits::setMtdPidTraits(const StMtdPidTraits* pid)
{
      mMatchFlag        = pid->matchFlag();
      mYLocal           = pid->yLocal();
      mZLocal           = pid->zLocal();
      mDeltaY           = pid->deltaY();
      mDeltaZ           = pid->deltaZ();
      mThetaLocal       = pid->thetaLocal();
      mTimeOfFlight     = pid->timeOfFlight();
      mExpTimeOfFlight  = pid->expTimeOfFlight();
      mPathLength       = pid->pathLength();
      mBeta             = pid->beta();
      mPosition         = pid->position();

      mSigmaMuon        = pid->sigmaMuon(); 
      mProbMuon         = pid->probMuon(); 
}

StMtdPidTraits* StMuMtdPidTraits::createMtdPidTraits() const
{
   StMtdPidTraits* traits = new StMtdPidTraits();
   traits->setMatchFlag(mMatchFlag);
   traits->setYLocal(mYLocal);
   traits->setZLocal(mZLocal);
   traits->setDeltaY(mDeltaY);
   traits->setDeltaZ(mDeltaZ);
   traits->setThetaLocal(mThetaLocal);
   traits->setTimeOfFlight(mTimeOfFlight);
   traits->setExpTimeOfFlight(mExpTimeOfFlight);
   traits->setPathLength(mPathLength);
   traits->setBeta(mBeta);
   traits->setPosition(mPosition);
   traits->setSigmaMuon(mSigmaMuon);
   traits->setProbMuon(mProbMuon);

   return traits;
}


StThreeVectorF&
StMuMtdPidTraits::position() { return mPosition; }

const StThreeVectorF&
StMuMtdPidTraits::position() const { return mPosition; }

void
StMuMtdPidTraits::setPosition(const StThreeVectorF& pos) { mPosition = pos; }
