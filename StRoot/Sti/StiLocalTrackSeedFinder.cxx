//StiLocalTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//10/01

//scl
#include "StThreeVectorF.hh"
#include "StThreeVector.hh"

//StiGui
#include "StiGui/StiDisplayManager.h"
#include "StiGui/StiRootDrawableHits.h"

//Sti
#include "StiHit.h"
#include "StiHitContainer.h"
#include "StiKalmanTrack.h"
#include "StiDetector.h"
#include "StiPlacement.h"
#include "StiDetectorContainer.h"
#include "StiLocalTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiDetector&);

StiLocalTrackSeedFinder::StiLocalTrackSeedFinder(StiDetectorContainer* det,
						 StiHitContainer* hits,
						 Sti2HitComboFilter* filt)
    : StiTrackSeedFinder(det, hits, filt), mHitVec(0)
{
    mMessenger <<"StiLocalTrackSeedFinder::StiLocalTrackSeedFinder()"<<endl;
}

StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()
{
    mMessenger <<"StiLocalTrackSeedFinder::~StiLocalTrackSeedFinder()"<<endl;
}

bool StiLocalTrackSeedFinder::hasMore()
{
    mMessenger <<"StiLocalTrackSeedFinder::hasMore()"<<endl;
    bool val = (mCurrentDet>=mDetVec.begin() && mCurrentDet<mDetVec.end())
	&& (mCurrentHit>=mHitVec->begin() && mCurrentHit<mHitVec->end());
    mMessenger <<"\t returning:\t"<<val<<endl;
    return val;
}

void StiLocalTrackSeedFinder::reset()
{
    mMessenger <<"StiLocalTrackSeedFinder::reset()"<<endl;
    //Set to first detector
    mCurrentDet = mDetVec.begin();

    //Set to hits for detector
    initHitVec();
    
    //Cleanup the base-class
    StiTrackSeedFinder::reset();
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::reset()"<<endl;
}

void StiLocalTrackSeedFinder::initHitVec()
{
    mMessenger <<"StiLocalTrackSeedFinder::initHitVec()"<<endl;
    bool go=true;
    while (mCurrentDet<mDetVec.end() && go) {
	mMessenger <<"\tCurrent detector: "<<**mCurrentDet<<endl;
	mHitVec = &(mhitstore->hits(*mCurrentDet));
	if (mHitVec->empty()==false) {
	    go=false;
	    mCurrentHit = mHitVec->begin();
	    mMessenger <<"\tStopping on this detector"<<endl;
	}
	else {
	    mMessenger <<"\tGo to next detector"<<endl;
	    ++mCurrentDet;
	}
    }
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::initHitVec()"<<endl;
}

void StiLocalTrackSeedFinder::increment()
{
    mMessenger <<"StiLocalTrackSeedFinder::increment()"<<endl;
    //Check for hit increment:
    if ( mCurrentHit<mHitVec->end() ) {
	mMessenger <<"\tincrementing mCurrentHit"<<endl;
	++mCurrentHit;
    }
    
    //Check to see if we went past the end
    if ( mCurrentHit<mHitVec->end()==false) {
	mMessenger <<"\tincrementing mCurrentDet"<<endl;
	++mCurrentDet;
	mMessenger <<"\tcalling initHitVec()"<<endl;
	initHitVec();
    }
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::increment()"<<endl;
    
}

StiKalmanTrack* StiLocalTrackSeedFinder::next()
{
    mMessenger <<"StiLocalTrackSeedFinder::next()"<<endl;
    mMessenger <<"\t Using hits from: "<<**mCurrentDet<<endl;
    StiKalmanTrack* track = 0;

    //For now, do one hit at a time, regardless of validity
    mdrawablehits->clear();
    track =makeTrack(*mCurrentHit);
    increment();
    
    /*
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
    */
    
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::next()"<<endl;
    return track;
}

StiKalmanTrack* StiLocalTrackSeedFinder::makeTrack(StiHit* hit)
{
    mMessenger <<"StiLocalTrackSeedFinder::makeTrack()"<<endl;
    StiKalmanTrack* track = 0;
    const StThreeVectorF& pos = hit->globalPosition();

    //mdrawablehits->push_back(StThreeVector<double>(pos.x(), pos.y(), pos.z() ));
    mdrawablehits->push_back( pos.x() );
    mdrawablehits->push_back( pos.y() );
    mdrawablehits->push_back( pos.z() );

    mdrawablehits->fillHitsForDrawing();

    mDetStore->setToDetector(*mCurrentDet);
    StiDisplayManager::instance()->draw();
    StiDisplayManager::instance()->update();
    
    mMessenger <<"\t leaving StiLocalTrackSeedFinder::makeTrack()"<<endl;
    return track;
}

void StiLocalTrackSeedFinder::addLayer(StiDetector* det)
{
    mDetVec.push_back(det);
    sort(mDetVec.begin(), mDetVec.end(), RPhiLessThan());
}


//Non members

//sort in descending order in radius, and ascending order in phi
bool RPhiLessThan::operator()(const StiDetector* lhs, const StiDetector* rhs)
{
    StiPlacement* lhsp = lhs->getPlacement();
    StiPlacement* rhsp = rhs->getPlacement();
    
    if (lhsp->getCenterRadius()<rhsp->getCenterRadius())
	return false;
    else if (lhsp->getCenterRadius()>rhsp->getCenterRadius()) 
	return true;
    else
	return (lhsp->getCenterRefAngle()<rhsp->getCenterRefAngle());
}

void StiLocalTrackSeedFinder::build()
{
    return;
}

void StiLocalTrackSeedFinder::print() const
{
    mMessenger <<"StiLocalTrackSeedFinder Detectors:\n";
    
    for (vector<StiDetector*>::const_iterator it=mDetVec.begin(); it!=mDetVec.end(); ++it) {
	mMessenger << **it <<endl;
    }
}
