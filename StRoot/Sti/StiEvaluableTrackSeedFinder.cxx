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

//StEvent
#include "StEventTypes.h"

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
#include "StiStTrackFilter.h"
#include "StiGeometryTransform.h"
#include "StiKalmanTrack.h"
#include "StiEvaluableTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiHit&);

StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(StAssociationMaker* assoc)
    : mAssociationMaker(assoc), mevent(0), mMcEvent(0)
{
    cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder()"<<endl;
    if (!assoc) {
	cout <<"\tERROR:\tAssociationMaker==0.  Undefined Behavior"<<endl;
    }
}

StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()
{
    cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()"<<endl;
}

void StiEvaluableTrackSeedFinder::setEvent(StEvent* evt, StMcEvent* mcevt) 
{
    mevent = evt;
    mMcEvent = mcevt;

    if (mcevt!=0) {
	//Get StMcTrack list from StMcEvent
	cout <<"StiEvaluableTrackSeedFinder::setEvent().  GetMcTrackContainer"<<endl;
	StSPtrVecMcTrack& tracks = mMcEvent->tracks();
	mBeginMc = tracks.begin();
	mEndMc = tracks.end();
	mCurrentMc = mBeginMc;
    }
    
    return;
}

void StiEvaluableTrackSeedFinder::addStTrackFilter(StiStTrackFilter* filter)
{
    mfiltervector.push_back(filter);
    return;
}

void StiEvaluableTrackSeedFinder::setStTrackType(StTrackType thetype)
{
    mtype = thetype;
    return;
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
    StiEvaluableTrack* track = mfactory->getObject();
    track->reset();
  
    mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
    if (!mcToStTrackMap) {
	cout <<"StiEvaluableTrackSeedFinder::makeTrack(StMcTrack*).  ERROR:\tMcTrackMap==0"<<endl;
	return track;
    }

    pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range = mcToStTrackMap->equal_range(mcTrack);
    /*
      if (range.first==mcToStTrackMap->end()) {
      cout <<"McTrack not found in map?"<<endl;
      }
      if ( (*(range.first)).first != mcTrack) {
      cout <<"equal_range.first != mcTrack???"<<endl;
      }
    */
    
    //Find bestTrack from association (linear search)
    //cout <<"New Track"<<endl;
    BestCommonHits theBest = for_each(range.first, range.second, BestCommonHits());
    StTrackPairInfo* bestPair = theBest.pair();
    
    if (!bestPair) {
	//cout <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack(StMcTrack* mcTrack) ERROR:\t";
	//cout <<"BestPair==0.  Abort"<<endl;
	return 0;
    }
    else {
	cout <<"Match Found, commonTpcHits:\t"<<bestPair->commonTpcHits()<<endl;
    }
    track->setStTrackPairInfo(bestPair);
    //ATTENTION CLAUDE: Uncomment the following to seed KalmanTrack and investigate problems!
    StiGeometryTransform::instance()->operator()(bestPair->partnerTrack(), track);
    
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

