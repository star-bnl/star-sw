///\File StiEvaluableTrackSeedFinder.cxx
///\author M.L. Miller (Yale Software) 04/01
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
//Sti
#include "Sti/Base/Messenger.h"
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
#include "StiHitContainer.h"
#include "StiEvaluableTrack.h"
#include "StiKalmanTrack.h"
#include "StiKalmanTrackNode.h"
#include "StiMapUtilities.h"
#include "StiEvaluableTrackSeedFinder.h"

ostream& operator<<(ostream&, const StiHit&);

/*! We require a valid pointer to an StAssociationMaker object.  All other constructor
  types are excplicity prohibited.  It is assumed, however, that the StAssociationMaker
  object is owned by some other scope.
*/
StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder(const string& name,
																												 Factory<StiKalmanTrack>* trackFactory,
																												 StiHitContainer      * hitContainer,
																												 StiDetectorContainer * detectorContainer,
																												 StAssociationMaker   * assoc)
	: StiTrackSeedFinder(name,trackFactory,hitContainer,detectorContainer),
		mAssociationMaker(assoc), mMcEvent(0), mTpcHitFilter(0),
		mLowerBound(5), mMaxHits(6)
{
  cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder() - INFO - Started"<<endl;
  if (!assoc) 
    throw runtime_error("StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder() - FATAL - assoc==0");
  mTpcHitFilter = new StTpcPadrowHitFilter();
  cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder() - INFO - Done"<<endl;
}

StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()
{
  cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder() - INFO - Started"<<endl;
  if (mTpcHitFilter) {
    delete mTpcHitFilter;
    mTpcHitFilter=0;
  }
  cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder() - INFO - Done"<<endl;
}

/*! This call is inherited from StiTrackSeedFinder but does not make much sense in the context
  of evaluable seeds.  That is, the internal state cannot be reset without a call to
  setEvent().
*/
void StiEvaluableTrackSeedFinder::reset()
{}

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
  _messenger <<"StiEvaluableTrackSeedFinder::setEvent().  GetMcTrackContainer"<<endl;
  StSPtrVecMcTrack& tracks = mMcEvent->tracks();
  mBeginMc = tracks.begin();
  mEndMc = tracks.end();
  mCurrentMc = mBeginMc;
  
  return;
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
  StiEvaluableTrack* track = dynamic_cast<StiEvaluableTrack*>(_trackFactory->getInstance());
  if (!track) 
    throw runtime_error("StiEvaluableTrackSeedFinder::makeTrack() - FATAL - StiEvaluableTrack* track==0");
  track->reset();
  mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
  if (!mcToStTrackMap) 
    throw runtime_error("StiEvaluableTrackSeedFinder::makeTrack() - FATAL - mcToStTrackMap==0");
  pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range = mcToStTrackMap->equal_range(mcTrack);
  if (range.first==range.second) 
    {
      //These return values are now caught before exiting the seedFinder control
      //_messenger <<"StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack() Error:\t";
      //_messenger <<"No valid range found.  Abort"<<endl;
      return 0;
    }
  
  //Find bestTrack from association (linear search)
  //_messenger <<"New Track"<<endl;
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
  
  if (!bestPair) 
    throw runtime_error("StiEvaluableTrackSeedFinder::makeTrack() - FATAL - bestPair==0");
  _messenger <<"Match Found, commonTpcHits:\t"<<bestPair->commonTpcHits()<<endl;
  track->setStTrackPairInfo(bestPair);
  //Set StiDetectorContainer to layer corresponding to
  //the innermost point on the track seed
  StiKalmanTrackNode* node = track->getInnerMostNode(); //Should return innermost
  if (!node) 
    throw runtime_error("StiEvaluableTrackSeedFinder::makeTrack() - FATAL - node==0");
  _detectorContainer->setToDetector(node->getHit()->detector());
  return track;
}

BestCommonHits::BestCommonHits() : mMostCommon(10), mPair(0)
{}

void BestCommonHits::operator()(const McToStPair_t& rhs)
{
  if (rhs.second->commonTpcHits()>mMostCommon) { //update, remember
    mMostCommon = rhs.second->commonTpcHits();
    mPair = rhs.second;
  }
}

void StiEvaluableTrackSeedFinder::initialize()
{

}
