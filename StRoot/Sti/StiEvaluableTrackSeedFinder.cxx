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
//#include "StEventTypes.h"

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
#include "StiEvaluableTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiHit&);

StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(StAssociationMaker* assoc)
    : mAssociationMaker(assoc), mMcEvent(0)
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

