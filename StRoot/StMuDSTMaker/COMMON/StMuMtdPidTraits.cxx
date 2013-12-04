#include "StMtdPidTraits.h"
#include "StMuMtdPidTraits.h"
#include "StMuMtdHit.h"

static const char rcsid[] = "$Id: StMuMtdPidTraits.cxx,v 1.1 2013/12/04 19:56:32 jdb Exp $";

ClassImp(StMuMtdPidTraits)

StMuMtdPidTraits::StMuMtdPidTraits() 
{
      mMatchFlag     = 0;
      mYLocal        = -999.;
      mZLocal        = -999.;
      mThetaLocal    = -999.;
      mTimeOfFlight  = -999.;
      mPathLength    = -999.;
      mBeta          = -999.;
      mPosition.set(0,0,0);

      mSigmaMuon     = -999.; 
      mProbMuon      = -999.; 
}

StMuMtdPidTraits::~StMuMtdPidTraits() { /* noop */ }


void StMuMtdPidTraits::setMtdPidTraits(const StMtdPidTraits* pid)
{
      mMatchFlag     = pid->matchFlag();
      mYLocal        = pid->yLocal();
      mZLocal        = pid->zLocal();
      mThetaLocal    = pid->thetaLocal();
      mTimeOfFlight  = pid->timeOfFlight();
      mPathLength    = pid->pathLength();
      mBeta          = pid->beta();
      mPosition      = pid->position();

      mSigmaMuon     = pid->sigmaMuon(); 
      //mProbMuon      = pid->probMuon(); 
}

StMtdPidTraits* StMuMtdPidTraits::createMtdPidTraits() const
{
   StMtdPidTraits* traits = new StMtdPidTraits();
   traits->setMatchFlag(mMatchFlag);
   traits->setYLocal(mYLocal);
   traits->setZLocal(mZLocal);
   traits->setThetaLocal(mThetaLocal);
   traits->setTimeOfFlight(mTimeOfFlight);
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
