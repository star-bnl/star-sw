//StiEvaluableTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//04/01

//Std
#include <iostream>
#include <algorithm>
using std::for_each;
using std::equal_range;

#include <math.h>
#include <map>
#include <utility>
using std::pair;

//SCL
#include "StGetConfigValue.hh"

//StMcEvent
#include "StMcEventTypes.hh"

//Association
//#include "StAssociationMaker/StTrackPairInfo.hh"
//#include "StAssociationMaker/StAssociationMaker.h"

//Sti
#include "StiIOBroker.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiHitContainer.h"
#include "StiEvaluableTrack.h"
#include "StiGeometryTransform.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiMapUtilities.h"
#include "StiEvaluableTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiHit&);

/*! We require a valid pointer to an StAssociationMaker object.  All other constructor
  types are excplicity prohibited.  It is assumed, however, that the StAssociationMaker
  object is owned by some other scope.
*/
StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(StAssociationMaker* assoc, StiHitContainer* hc)
    : Observer(StiIOBroker::instance()),
      StiSeedFinder(hc),
      mAssociationMaker(assoc), mMcEvent(0), mTpcHitFilter(0),
      mIOBroker(StiIOBroker::instance()),
      mLowerBound(0), mMaxHits(0)
{
    cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder()"<<endl;
    if (!assoc) {
	cout <<"\tERROR:\tAssociationMaker==0.  Undefined Behavior"<<endl;
    }

    mSubject->attach(this);
    mTpcHitFilter = new StTpcPadrowHitFilter();
    getNewState();

}

StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()
{
    cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()"<<endl;
    if (mTpcHitFilter) {
	delete mTpcHitFilter;
	mTpcHitFilter=0;
    }

    if (mSubject) {
	mSubject->detach(this);
    }
}

/*! This call is inherited from StiSeedFinder but does not make much sense in the context
  of evaluable seeds.  That is, the internal state cannot be reset without a call to
  setEvent().
*/
void StiEvaluableTrackSeedFinder::reset()
{
}

/*! This should be called once per event.  The call to setEVent internally initializes
  the seed finder for the event.  Without this call, the behavior of hasMore() and next()
  is undefined.
*/
void StiEvaluableTrackSeedFinder::setEvent(StMcEvent* mcevt) 
{
    mMcEvent = mcevt;
    if (mcevt==0) {
	cout <<"StiEvaluableTrackSeedFinder::setEvent(). ERROR:\tmcEvent==0"<<endl;
	return;
    }

    //Get StMcTrack list from StMcEvent
    mMessenger <<"StiEvaluableTrackSeedFinder::setEvent().  GetMcTrackContainer"<<endl;
    StSPtrVecMcTrack& tracks = mMcEvent->tracks();
    mBeginMc = tracks.begin();
    mEndMc = tracks.end();
    mCurrentMc = mBeginMc;
    
    return;
}

void StiEvaluableTrackSeedFinder::getNewState()
{
    //cout <<"StiEvaluableTrackSeedFinder::getNewState()"<<endl;
    //cout <<"\n\n ------------------- StiIOBroker ----------------------- \n\n"<<*mIOBroker<<endl;

    mLowerBound = mIOBroker->etsfLowerBound();
    mMaxHits = mIOBroker->etsfMaxHits();
    mLowerBound = mIOBroker->etsfLowerBound();
    //cout <<"maxHits: "<<mMaxHits<<"\tlowerBound: "<<mLowerBound<<endl;
    mTpcHitFilter->getNewState();
}

/*! A call to hasMore() simply checks if there are more seeds to be generated.
  It does not implement any increment or decrement calls, and thus may be called without
  ever changing the internal state of the seed finder.
*/
bool StiEvaluableTrackSeedFinder::hasMore()
{
    return (mCurrentMc!=mEndMc);
}

/*! A call to next() constructs a seed from the current m.c. track and increments
  a pointer to the next available m.c. track.  The generation of the seed itself is
  performed by the private makeTrack() method.
*/
StiKalmanTrack* StiEvaluableTrackSeedFinder::next()
{
    StiKalmanTrack* track = 0;
    while (track==0 && hasMore()) {
	track = makeTrack(*(mCurrentMc++));
    }
    return track;
}

/*! This function is the heart of the seed-finder.  It finds the best associated match
  from the StAssociationMaker instance and initializes the StiKalmanTrack state with
  the parameters from the m.c. track and the hits from the StGlobalTrack.
*/
StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack)
{
    StiEvaluableTrack* track = dynamic_cast<StiEvaluableTrack*>(mFactory->getInstance());
    if (!track) return 0;
    
    track->reset();
    
    mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
    if (!mcToStTrackMap) {
	mMessenger <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack*).  ERROR:\t";
	mMessenger <<"McTrackMap==0"<<endl;
	return 0;
    }
    
    pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range =
	mcToStTrackMap->equal_range(mcTrack);
    if (range.first==range.second) {
	//These return values are now caught before exiting the seedFinder control
	//mMessenger <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack() Error:\t";
	//mMessenger <<"No valid range found.  Abort"<<endl;
	return 0;
    }
    
    //Find bestTrack from association (linear search)
    //mMessenger <<"New Track"<<endl;
    mBestCommon.reset();
    mBestCommon.setLowerBound(mLowerBound);

    //Temp (MLM) replace with a loop
    // BestCommonHits theBest = for_each(range.first, range.second, mBestCommon );    
    // StTrackPairInfo* bestPair = theBest.pair();

    //Start kludge here
    //mcTrackMapType::iterator first = range.first;
    //mcTrackMapType::iterator second = range.second;    
    //mcTrackMapType::iterator end = second++;    
    StTrackPairInfo* bestPair=0;
    unsigned int mostCommon=mLowerBound;

    //for (mcTrackMapType::iterator it=first; it!=end; ++it) {	
    for (mcTrackMapType::iterator it=range.first; it!=range.second; ++it) {	
	StTrackPairInfo* info = (*it).second;
	if (info->commonTpcHits()>mostCommon) { //update, remember
	    mostCommon = info->commonTpcHits();
	    bestPair = info;
	}
    }
    //End kludge here;
    
    if (!bestPair) {
	//These return values are now caught before exiting the seedFinder control
	//mMessenger <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack) ERROR:\t";
	//mMessenger <<"BestPair==0.  Abort"<<endl;
	return 0;
    }
    else {
	mMessenger <<"Match Found, commonTpcHits:\t"<<bestPair->commonTpcHits()<<endl;
    }
    track->setStTrackPairInfo(bestPair);

    //fitlered
    StiGeometryTransform::instance()->operator()(mHitStore, bestPair->partnerTrack(),
						 track, mMaxHits, mTpcHitFilter);

    //Set StiDetectorContainer to layer corresponding to
    //the innermost point on the track seed
    StiKalmanTrackNode* node = track->getInnerMostNode(); //Should return innermost
    if (!node) {
	mMessenger <<"StiEvaluableTrackSeedFinder::makeTrack(). ERROR:\t";
	mMessenger <<"node==0.  return;"<<endl;
	return 0;
    }
    else {
	StiDetector* layer = node->getHit()->detector();
	StiDetectorContainer::instance()->setToDetector(layer);
    }
    
    return track;
}

BestCommonHits::BestCommonHits() : mMostCommon(10), mPair(0)
{
}

void BestCommonHits::operator()(const McToStPair_t& rhs)
{
    if (rhs.second->commonTpcHits()>mMostCommon) { //update, remember
	mMostCommon = rhs.second->commonTpcHits();
	mPair = rhs.second;
    }
}

