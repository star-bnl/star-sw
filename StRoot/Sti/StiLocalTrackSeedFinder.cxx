//StiLocalTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//10/01

//StiGui
#include "StiGui/StiRootDrawableHits.h"

//Sti
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiLocalTrackSeedFinder.h"

StiLocalTrackSeedFinder::StiLocalTrackSeedFinder(StiDetectorContainer* det,
						 StiHitContainer* hits, Sti2HitComboFilter* filt)
    : StiTrackSeedFinder(det, hits, filt), mHitVec(0)
{
    cout <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder()"<<endl;
}

StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
    cout <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()"<<endl;
}

bool StiLocalTrackSeedFinder::hasMore()
{
    return (mCurrentDet>=mDetVec.begin() && mCurrentDet<mDetVec.end())
	&& (mCurrentHit>=mHitVec->begin() && mCurrentHit<mHitVec->end());
}

void StiLocalTrackSeedFinder::build()
{
    return;
}

void StiLocalTrackSeedFinder::reset()
{
    //Set to first detector
    mCurrentDet = mDetVec.begin();
    //Set to hits for detector
    mHitVec = &(mhitstore->hits(*mCurrentDet));
    
    //Cleanup the base-class
    StiTrackSeedFinder::reset();
}

bool StiLocalTrackSeedFinder::increment()
{
    //Check for hit increment:
    if ( mCurrentHit<mHitVec->end() ) {
	++mCurrentHit;
    }
    
    //Check to see if that's still valid:
    if ( (mCurrentHit<mHitVec->end())==false ) {
	
	//Oops, out of hits, try to increment the detector
	if (mCurrentDet<mDetVec.end()) {
	    ++mCurrentDet;
	}
	
	//Check to see if it's valid:
	if ( (mCurrentDet<mDetVec.end())==false) {
	    //Done
	    return false;
	}
	
	else {
	    mHitVec = &(mhitstore->hits(*mCurrentDet));
	    mCurrentHit = mHitVec->begin();
	    return true;
	}
    }
    else {
	return true;
    }
}

StiKalmanTrack* StiLocalTrackSeedFinder::next()
{
    StiKalmanTrack* track = 0;
    bool go=true;
    while (go) {
	mdrawablehits->clear();
	track = makeTrack(*mCurrentHit);
	if (track) {
	    //Increment, return
	    increment();
	    go=false;
	}
	else {
	    go=increment();
	}
    }
    
    return track;
}

StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(const StiHit* hit)
{
    StiKalmanTrack* track = 0;
    return track;
}

void StiLocalTrackSeedFinder::addLayer(StiDetector* det)
{
    mDetVec.push_back(det);
    sort(mDetVec.begin(), mDetVec.end(), RPhiLessThan());
}


//Non members

bool RPhiLessThan::operator()(const StiDetector* lhs, const StiDetector* rhs)
{
    StiPlacement* lhsp = lhs->getPlacement();
    StiPlacement* rhsp = rhs->getPlacement();
    
    if (lhsp->getCenterRadius()<rhsp->getCenterRadius())
	return true;
    else if (lhsp->getCenterRadius()>rhsp->getCenterRadius()) 
	return false;
    else
	return (lhsp->getCenterRadius()<rhsp->getCenterRadius());
}
