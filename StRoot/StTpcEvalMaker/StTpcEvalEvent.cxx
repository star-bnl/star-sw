//-------------------------------------------------
// For StTpcEvalMaker
//-------------------------------------------------
// author: milton toy
//         
// additions: manuel cbs
//-------------------------------------------------
// class definitions for MatchedHitPair
//                       TrackInfo
//                       rcTrackInfo:TrackInfo
//                       mcTrackInfo:TrackInfo
//                       MatchedTrackPair
//-------------------------------------------------
#include <stdlib.h>
#include <math.h>

#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StLorentzVectorF.hh"
#include "StThreeVectorD.hh"
#include "StPhysicalHelixD.hh"
#include "StEnumerations.h"

#include "StTpcDb/StTpcDb.h"
#include "StDbUtilities/StGlobalCoordinate.hh"
#include "StDbUtilities/StTpcLocalSectorCoordinate.hh"
#include "StDbUtilities/StTpcCoordinateTransform.hh"

//#include "StChain.h"
//#include "St_DataSet.h"
//#include "St_DataSetIter.h"

#include "StTpcHit.h"
#include "StGlobalTrack.h"
#include "StMcTpcHit.hh"
#include "StMcTrack.hh"

#include "StTpcEvalEvent.h"

#define USE_DATABASE
//-----------------------------------------------------------------------
    
MatchedHitPair::MatchedHitPair(StTpcDb* tpcDb)
    : mStTpcDb(tpcDb), mResolution(-999,-999,-999){ /*noop*/}

StThreeVectorF& MatchedHitPair::resolution() {return mResolution;}

StThreeVectorF& MatchedHitPair::resolution(const StMcTpcHit* mcHit,
					 const StTpcHit* rcHit) {
#ifdef USE_DATABASE
  // transform tpc hit positions from global to internal
  StTpcLocalSectorCoordinate rcLocalHit, mcLocalHit;

  StGlobalCoordinate rcGlobalHit(rcHit->position());
  StGlobalCoordinate mcGlobalHit(mcHit->position());

  StTpcCoordinateTransform mHitTransform(mStTpcDb);
  mHitTransform(rcGlobalHit,rcLocalHit);
  mHitTransform(mcGlobalHit,mcLocalHit);
  StThreeVector<double> tmp(rcLocalHit.position() - mcLocalHit.position());
  for(size_t i= 0; i<3; i++) mResolution(i) = tmp(i); // have to do this because we can't use templates...
  
#else // GLOBAL COORDINATES
  mResolution = mcHit->position() - rcHit->position();
#endif
  return mResolution;
}

// TrackInfo Implementation
//-----------------------------------------------------------------------
ClassImp(TrackInfo)

TrackInfo::TrackInfo()
    :  TObject(), mId(0), mHits(0), mMatchedHits(0), mMatchedTracks(0) {/*noop*/}
TrackInfo::~TrackInfo(){}
signed int TrackInfo::trackId() {return mId;}
unsigned int TrackInfo::numberOfHits() {return mHits;}
unsigned int TrackInfo::numberOfMatchedHits() {return mMatchedHits;}
unsigned int TrackInfo::numberOfMatchedTracks() {return mMatchedTracks;}
void TrackInfo::setId(Int_t id) {mId = id;}
void TrackInfo::setHits(UInt_t hits) {mHits = hits;}
void TrackInfo::setMatchedHits(UInt_t nm) {mMatchedHits = nm;}
void TrackInfo::setMatchedTracks(UInt_t nm) {mMatchedTracks = nm;}

// rcTrackInfo Implementation
//-----------------------------------------------------------------------
ClassImp(rcTrackInfo)

rcTrackInfo::rcTrackInfo()
    : TrackInfo(), mMomentum(-999,-999,-999){/*noop*/}
rcTrackInfo::~rcTrackInfo() { }
StThreeVectorD& rcTrackInfo::momentum() {return mMomentum;}
UInt_t rcTrackInfo::fitHits() {return mFitHits;}
void rcTrackInfo::setMomentum(StThreeVectorD mom) {mMomentum = mom;}
void rcTrackInfo::setFitHits(UInt_t fitHits) {mFitHits = fitHits;}


// mcTrackInfo Implementation
//-----------------------------------------------------------------------
ClassImp(mcTrackInfo)
mcTrackInfo::mcTrackInfo()
    : TrackInfo(), mFourMomentum(-999,-999,-999, -999){ /*noop*/}
mcTrackInfo::~mcTrackInfo() {  }
StLorentzVectorF& mcTrackInfo::fourMomentum() {return mFourMomentum;}
void mcTrackInfo::setFourMomentum(const StLorentzVectorF& mom) {mFourMomentum = mom;}


//-----------------------------------------------------------------------
ClassImp(MatchedTrackPair)

MatchedTrackPair::MatchedTrackPair()
    : TObject(), mCommonHits(0), mHitCounter(0),
      mMomentumResolution(-999,-999,-999),
      mSpatialResolution(-999,-999,-999),
      mSpatialResolutionRMS(-999,-999,-999) { }

MatchedTrackPair::~MatchedTrackPair() {/*noop*/}

mcTrackInfo* MatchedTrackPair::mcInfo() {return &mMcInfo;}

rcTrackInfo* MatchedTrackPair::rcInfo() {return &mRcInfo;}

unsigned int MatchedTrackPair::commonHits() {return mCommonHits;}

StThreeVectorF& MatchedTrackPair::momentumResolution() {return mMomentumResolution;}

StThreeVectorF& MatchedTrackPair::spatialResolution() {return mSpatialResolution;}

StThreeVectorF& MatchedTrackPair::spatialResolutionRMS() {return mSpatialResolutionRMS;}

void MatchedTrackPair::setMomentumResolution(StThreeVectorF& dp) { mMomentumResolution = dp; }

void MatchedTrackPair::setCommonHits(UInt_t nhits) { mCommonHits = nhits; }

void MatchedTrackPair::setSpatialResolution(StThreeVectorF& reso) { mSpatialResolution = reso; }

void MatchedTrackPair::setSpatialResolutionRMS(StThreeVectorF& rms) { mSpatialResolutionRMS = rms; }

void MatchedTrackPair::addHitResolution(StThreeVectorF& reso) {

  mSpatialResolution *= mHitCounter;
  mSpatialResolutionRMS *= mHitCounter;
  mHitCounter++;

  mSpatialResolution += reso;
  mSpatialResolution /= mHitCounter;

  // a very inelegant way to do this operation, but there aren't any
  // member functions of StThreeVectorF that can do it...
  // it's probably wrong anyways
  Float_t mx;
  mx = sqrt(mSpatialResolutionRMS.x()*mSpatialResolutionRMS.x()
	    + reso.x()*reso.x());
  mSpatialResolutionRMS.setX(mx);
  mx = sqrt(mSpatialResolutionRMS.y()*mSpatialResolutionRMS.y()
	    + reso.y()*reso.y());
  mSpatialResolutionRMS.setY(mx);
  mx = sqrt(mSpatialResolutionRMS.z()*mSpatialResolutionRMS.z()
	    + reso.z()*reso.z());
  mSpatialResolutionRMS.setZ(mx);

  mSpatialResolutionRMS /= mHitCounter;
}

//-----------------------------------------------------------------------
ClassImp(StTpcEvalEventHeader)
    
ClassImp(StTpcEvalEvent)

StTpcEvalEvent::StTpcEvalEvent() {
    mNumMatchedTracks = 0;
    mMatchedTracks = new TClonesArray("MatchedTrackPair",1000);
    
}
StTpcEvalEvent::~StTpcEvalEvent() {
    if (mMatchedTracks) delete mMatchedTracks;
    mMatchedTracks = 0;
    mNumMatchedTracks = 0;
}

void StTpcEvalEvent::Clear(Option_t* option) {
    if (mMatchedTracks) mMatchedTracks->Clear(option);
}

void StTpcEvalEvent::SetHeader(UInt_t geTrk, UInt_t globTrk, UInt_t gePTrk, UInt_t primTrk, UInt_t geTHit, UInt_t reTHit) {
    mHeader.setHeader(geTrk, globTrk, gePTrk, primTrk, geTHit, reTHit);
}

void StTpcEvalEvent::addTrackPair(MatchedTrackPair& pair) {
    TClonesArray& trks = *mMatchedTracks;
    new(trks[mNumMatchedTracks++]) MatchedTrackPair(pair);
}
