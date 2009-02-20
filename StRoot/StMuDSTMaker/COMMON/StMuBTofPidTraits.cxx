/***************************************************************************
 *
 * $Id: StMuBTofPidTraits.cxx,v 1.1 2009/02/20 17:05:59 tone421 Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMuBTofPidTraits.cxx,v $
 * Revision 1.1  2009/02/20 17:05:59  tone421
 * *** empty log message ***
 *
 *
 ***************************************************************************/
#include "StBTofPidTraits.h"
#include "StMuBTofPidTraits.h"

static const char rcsid[] = "$Id: StMuBTofPidTraits.cxx,v 1.1 2009/02/20 17:05:59 tone421 Exp $";

ClassImp(StMuBTofPidTraits)

StMuBTofPidTraits::StMuBTofPidTraits()
{
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

StMuBTofPidTraits::~StMuBTofPidTraits() { /* noop */ }

void StMuBTofPidTraits::setBTofPidTraits(const StBTofPidTraits* pid)
{
      mMatchFlag     = pid->matchFlag();
      mYLocal        = pid->yLocal();
      mZLocal        = pid->zLocal();
      mThetaLocal    = pid->thetaLocal();
      mTimeOfFlight  = pid->timeOfFlight();
      mPathLength    = pid->pathLength();
      mBeta          = pid->beta();

      mPosition      = pid->position();

      mSigmaElectron = pid->sigmaElectron(); 
      mSigmaPion     = pid->sigmaPion(); 
      mSigmaKaon     = pid->sigmaKaon(); 
      mSigmaProton   = pid->sigmaProton();  
      mProbElectron  = pid->probElectron(); 
      mProbPion      = pid->probPion(); 
      mProbKaon      = pid->probKaon(); 
      mProbProton    = pid->probProton();  
}

StBTofPidTraits* StMuBTofPidTraits::createBTofPidTraits()
{
   StBTofPidTraits* traits = new StBTofPidTraits();
   traits->setMatchFlag(mMatchFlag);
   traits->setYLocal(mYLocal);
   traits->setZLocal(mZLocal);
   traits->setThetaLocal(mThetaLocal);
   traits->setTimeOfFlight(mTimeOfFlight);
   traits->setPathLength(mPathLength);
   traits->setBeta(mBeta);
   traits->setPosition(mPosition);

   traits->setSigmaElectron(mSigmaElectron);
   traits->setSigmaPion(mSigmaPion);
   traits->setSigmaKaon(mSigmaKaon);
   traits->setSigmaProton(mSigmaProton);
   traits->setProbElectron(mProbElectron);
   traits->setProbPion(mProbPion);
   traits->setProbKaon(mProbKaon);
   traits->setProbProton(mProbProton);

   return traits;
}

StThreeVectorF&
StMuBTofPidTraits::position() { return mPosition; }

const StThreeVectorF&
StMuBTofPidTraits::position() const { return mPosition; }

void
StMuBTofPidTraits::setPosition(StThreeVectorF& pos) { mPosition = pos; }
