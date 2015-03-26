#include "StMuDSTMaker/COMMON/StMuMtdPidTraits.h"
#include "StMuDSTMaker/COMMON/StMuMtdHit.h"
#include "StarClassLibrary/PhysicalConstants.h"
#include "StMessMgr.h"

#include "StPicoConstants.h"
#include "StPicoMtdPidTraits.h"

ClassImp(StPicoMtdPidTraits)

//----------------------------------------------------------------------------------
StPicoMtdPidTraits::StPicoMtdPidTraits() : 
  mMtdHitChan(-1), mTrackIndex(-1), mMatchFlag(-1),
  mDeltaY(-999.), mDeltaZ(-999.), mDeltaTimeOfFlight(-999.), mBeta(-999.)
{
  // constructor
}

//----------------------------------------------------------------------------------
StPicoMtdPidTraits::StPicoMtdPidTraits(const StMuMtdHit * hit, 
				       const StMuMtdPidTraits* trait, 
				       const Int_t index)
{
  Int_t gchan = (hit->backleg()-1)*60 + (hit->module()-1)*12 + hit->cell();
  mMtdHitChan = (gchan > Pico::SHORTMAX) ? -1 : (Short_t) gchan;
  mTrackIndex         = (Short_t)index;
  mMatchFlag          = (Char_t) trait -> matchFlag();
  mDeltaY             = trait -> deltaY();
  mDeltaZ             = trait -> deltaZ();
  mDeltaTimeOfFlight  = trait -> timeOfFlight() - trait -> expTimeOfFlight();
  mBeta               = (trait->pathLength()/trait->expTimeOfFlight())*1e9/c_light;
}

//----------------------------------------------------------------------------------
StPicoMtdPidTraits::~StPicoMtdPidTraits()
{ 
  // dummy destructor
}


//----------------------------------------------------------------------------------
void StPicoMtdPidTraits::Print(const Char_t *option) const
{
  LOG_INFO << "Matched hit: backleg =  " << backleg() 
	   << ", module  = " << module()
	   << ", cell    = " << cell()
	   <<endm;
  LOG_INFO << "Matched track index = " << mTrackIndex << endm;
  LOG_INFO << "(DeltaY, DeltaZ, DeltaTOF, beta) = ("
	   << mDeltaY << ", "
	   << mDeltaZ << ", "
	   << mDeltaTimeOfFlight << ", "
	   << mBeta << ")" << endm;
}
