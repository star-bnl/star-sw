//StiEvaluableTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//04/01

//Std
#include <iostream>
#include <algorithm>
using std::for_each;

#include <math.h>
#include <map>
#include <utility>

//SCL
#include "StGetConfigValue.hh"

//StMcEvent
#include "StMcEventTypes.hh"
/// \dontinclude StMcEventTypes.hh

//Association
#include "StAssociationMaker/StTrackPairInfo.hh"
#include "StAssociationMaker/StAssociationMaker.h"

//Sti
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
StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(StAssociationMaker* assoc)
    : mAssociationMaker(assoc), mMcEvent(0), mFactory(0), mTpcHitFilter(0),
      mBuildPath("empty"), mBuilt(false), mLowerBound(0), mMaxHits(0)
{
    cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder()"<<endl;
    if (!assoc) {
	cout <<"\tERROR:\tAssociationMaker==0.  Undefined Behavior"<<endl;
    }
}

StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()
{
    cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()"<<endl;
    if (mTpcHitFilter) {
	delete mTpcHitFilter;
	mTpcHitFilter=0;
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
    cout <<"StiEvaluableTrackSeedFinder::setEvent().  GetMcTrackContainer"<<endl;
    StSPtrVecMcTrack& tracks = mMcEvent->tracks();
    mBeginMc = tracks.begin();
    mEndMc = tracks.end();
    mCurrentMc = mBeginMc;
    
    return;
}

/*! A call to build builds the internal state from the text file specified by
  buildPath.  If buildPath is not initialized, warning messages are streamed and
  behavior of the seed-finder object is undefined.
*/    
void StiEvaluableTrackSeedFinder::build()
{
    cout <<"StiEvalaubleTrackSeedFinder::build()"<<endl;
    if (mBuildPath=="empty") {
	cout <<"StiEvalaubleTrackSeedFinder::build(). ERROR:\tmBuildPath==empty.  ABORT"<<endl;
	return;
    }

    if (mBuilt) {
	cout <<"StiEvalaubleTrackSeedFinder::build(). ERROR:\talready built!.  ABORT"<<endl;
	return;
    }

    cout <<"StiEvalaubleTrackSeedFinder::build().  Build from:\t"<<mBuildPath<<endl;

    StGetConfigValue(mBuildPath.c_str(), "lowerBound", mLowerBound);
    if (mLowerBound==0) {
	cout <<"StiEvalaubleTrackSeedFinder::build(). ERROR:\t";
	cout <<"lowerBound==0.  ABORT"<<endl;
	return;
    }

    StGetConfigValue(mBuildPath.c_str(), "mMaxHits", mMaxHits);
    if (mMaxHits==0) {
	cout <<"StiEvaluableTrackSeedFinder::build(). ERROR:\t";
	cout <<"maxHits==0 Abort"<<endl;
	return;
    }

    string hitFilterType="empty";
    StGetConfigValue(mBuildPath.c_str(), "hitFilterType", hitFilterType);
    if (hitFilterType=="empty") {
	cout <<"StiEvalaubleTrackSeedFinder::build(). ERROR:\tmhitFilterType==empty.  ABORT"<<endl;
	return;
    }
    else if (hitFilterType=="StTpcPadrowHitFilter") {
	mTpcHitFilter = new StTpcPadrowHitFilter();
    }
    else {
	cout <<"StiEvalaubleTrackSeedFinder::build(). ERROR:t";
	cout <<"unkown hitfilter type: "<<hitFilterType<<".  ABORT"<<endl;
	return;
    }
    mTpcHitFilter->build(mBuildPath);

    cout <<"lowerBound:\t"<<mLowerBound<<"\tmaxHits:\t"<<mMaxHits<<endl;
    mBuilt=true;
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
    StiEvaluableTrack* track = dynamic_cast<StiEvaluableTrack*>(mFactory->getObject());
    if (!track) return 0;
    
    track->reset();
    
    mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
    if (!mcToStTrackMap) {
	cout <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack*).  ERROR:\t";
	cout <<"McTrackMap==0"<<endl;
	return 0;
    }
    
    pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range =
	mcToStTrackMap->equal_range(mcTrack);
    if (range.first==mcToStTrackMap->end()) {
	//These return values are now caught before exiting the seedFinder control
	//cout <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack() Error:\t";
	//cout <<"No valid range found.  Abort"<<endl;
	return 0;
    }
    
    //Find bestTrack from association (linear search)
    //cout <<"New Track"<<endl;
    mBestCommon.reset();
    mBestCommon.setLowerBound(mLowerBound);
    
    BestCommonHits theBest = for_each(range.first, range.second, mBestCommon );
    
    StTrackPairInfo* bestPair = theBest.pair();
    
    if (!bestPair) {
	//These return values are now caught before exiting the seedFinder control
	//cout <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack) ERROR:\t";
	//cout <<"BestPair==0.  Abort"<<endl;
	return 0;
    }
    else {
	cout <<"Match Found, commonTpcHits:\t"<<bestPair->commonTpcHits()<<endl;
    }
    track->setStTrackPairInfo(bestPair);

    //fitlered
    StiGeometryTransform::instance()->operator()(bestPair->partnerTrack(),
						 track, mMaxHits, mTpcHitFilter);

    //Set StiDetectorContainer to layer corresponding to
    //the innermost point on the track seed
    StiKalmanTrackNode* node = track->getLastNode(); //Should return innermost
    if (!node) {
	cout <<"StiEvaluableTrackSeedFinder::makeTrack(). ERROR:\t";
	cout <<"node==0.  return;"<<endl;
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
    //cout <<"\t"<<rhs.second->commonTpcHits()<<"\t"<<mMostCommon<<endl;
    if (rhs.second->commonTpcHits()>mMostCommon) { //update, remember
	mMostCommon = rhs.second->commonTpcHits();
	mPair = rhs.second;
	//cout <<"\t\t\tBetter, remember"<<endl;
    }
}

