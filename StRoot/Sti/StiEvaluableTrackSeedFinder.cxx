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

StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(StAssociationMaker* assoc)
    : mAssociationMaker(assoc), mMcEvent(0), mFactory(0), mTpcHitFilter(0),
      mBuildPath("empty"), mBuilt(false)
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

//No-op
void StiEvaluableTrackSeedFinder::reset()
{
}

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
    
    mBuilt=true;
}

bool StiEvaluableTrackSeedFinder::hasMore()
{
    return (mCurrentMc!=mEndMc);
}

StiKalmanTrack* StiEvaluableTrackSeedFinder::next()
{
    return makeTrack(*(mCurrentMc++));
}

StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack)
{
    StiEvaluableTrack* track = dynamic_cast<StiEvaluableTrack*>(mFactory->getObject());
    if (!track) return 0;
    
    track->reset();
  
    mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
    if (!mcToStTrackMap) {
	cout <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack*).  ERROR:\t";
	cout <<"McTrackMap==0"<<endl;
	return track;
    }

    pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range = mcToStTrackMap->equal_range(mcTrack);
    if (range.first==mcToStTrackMap->end()) {
	cout <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack() Error:\t";
	cout <<"No valid range found.  Abort"<<endl;
	return 0;
    }
    
    //Find bestTrack from association (linear search)
    //cout <<"New Track"<<endl;
    BestCommonHits theBest = for_each(range.first, range.second, BestCommonHits());

    StTrackPairInfo* bestPair = theBest.pair();
    
    if (!bestPair) {
	cout <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack) ERROR:\t";
	cout <<"BestPair==0.  Abort"<<endl;
	return 0;
    }
    else {
	cout <<"Match Found, commonTpcHits:\t"<<bestPair->commonTpcHits()<<endl;
    }
    track->setStTrackPairInfo(bestPair);

    //StiGeometryTransform::instance()->operator()(bestPair->partnerTrack(), track); //unfiltered
    StiGeometryTransform::instance()->operator()(bestPair->partnerTrack(), track, mTpcHitFilter); //fitlered

    //Set StiDetectorContainer to layer corresponding to the innermost point on the track seed
    StiKalmanTrackNode* node = track->getLastNode(); //Should return innermost
    if (!node) {
	cout <<"StiEvaluableTrackSeedFinder::makeTrack(). ERROR!\tnode==0.  return;"<<endl;
    }
    else {
	StiDetector* layer = node->getHit()->detector();
	StiDetectorContainer::instance()->setToDetector(layer);
    }
    
    return track;
}

BestCommonHits::BestCommonHits() : mMostCommon(0), mPair(0)
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

