//StiTrackAssociator.cxx
//M.L. Miller (Yale Software)
//11/01

//Std
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <utility>

using namespace std;

//Sti
#include "Sti/StiHit.h"
#include "Sti/StiKalmanTrack.h"
#include "Sti/StiKalmanTrackNode.h"

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//SCL
#include "StThreeVector.hh"
#include "StThreeVectorF.hh"
#include "StThreeVectorD.hh"

//StiEvaluator
#include "StiTrackAssociator.h"

StiTrackAssociator::StiTrackAssociator(StAssociationMaker* a)
    : mAssociationMaker(a)
{
    cout <<"StiTrackAssociator::StiTrackAssociator()"<<endl;
    if (!mAssociationMaker) {
	cout <<"StiTrackAssociator::StiTrackAssociatorr(). ERROR:\t"
	     <<"mAssociationMaker null.  Undefined behavior"<<endl;
    }
}

StiTrackAssociator::~StiTrackAssociator()
{
    cout <<"StiTrackAssociator::StiTrackAssociator()"<<endl;
}

/*! AssocPair::first gives the StTrackPairInfo from the standard MC <--> Global Association 
  AssocPair::second gives the trackPing from the ITTF <--> MC Association 
  However, AssocPair::first->partnerMcTrack() should be == to AssocPair::second->mcTrack 
  That is, we define the 3-way association by synchronizing on the same McTrack.
  It is possible that the AssocPair::first can be null.  This happens when the current
  STAR global tracking fails to find the given MC track.  If AssocPair::second.mcTrack is null,
  this means that the 3-way association failed.
*/
StiTrackAssociator::AssocPair StiTrackAssociator::associate(StiKalmanTrack* stiTrack)
{
    //We need this as a dummy return to flag failed association
    trackPing nullPing;
    
    if (!mAssociationMaker) {
	cout <<"StiTrackAssociator::associate(StiKalmanTrack*). ERROR:\t"
	     <<"mAssociationMaker null.  Abort"<<endl;
	return AssocPair(0, nullPing);
    }

    mMap.clear();
    
    //Loop on the track hits:
    loopOnHits(stiTrack);
    
    //Choose the best track:
    trackPing* ping = chooseBestAssociation();
    if (!ping) {
	cout <<"Error, bestPing==0 Abort"<<endl;
	return AssocPair(0, nullPing);
    }

    //Now we have the best ping, find the best TrackPairInfo for this MC track:
    StTrackPairInfo* info = findBestTrackPair(ping);
    //if (!info) {
    //cout <<"Error, info==0.  Abort"<<endl;
    //return AssocPair(0, nullPing);
    //}
    
    return AssocPair(info, *ping);
}

void StiTrackAssociator::loopOnHits(const StiKalmanTrack* track)
{
    StiKalmanTrackNode* lastNode = track->getLastNode();
    unsigned int nHits=0;
    
    while (!lastNode->isRoot() ) {
	const StiHit* stiHit = lastNode->getHit();
	if (stiHit) {
	    //lookup StMcHit association:
	    associate(stiHit);
	    ++nHits;
	}
	lastNode = dynamic_cast<StiKalmanTrackNode*>(lastNode->getParent());
	if (!lastNode) {
	    cout <<"StiTrackAssociator::associate(StiKalmanTrack*). ERROR:\t"
		 <<"Downcast failed.  Seg-fault coming"<<endl;
	}
    }
    // cout <<"\tLooped on: "<<nHits<<" hits"<<endl;
}

trackPing* StiTrackAssociator::chooseBestAssociation()
{
    // cout <<"\tChooose best Association:  Ping map:"<<endl;
    unsigned int bestPings = 0;
    trackPing* theBestPing=0;
    
    for (TrackPingMap::iterator it=mMap.begin(); it!=mMap.end(); ++it) {
	// cout <<"\t &McTrack: "<<(*it).first<<" nPings Tpc: "<<(*it).second.nPingsTpc<<endl;
	
	const trackPing& ping = (*it).second;
	unsigned int nPings = ping.nPingsTpc+ping.nPingsSvt+ping.nPingsFtpc;
	if (nPings>bestPings) {
	    theBestPing = &((*it).second);
	    bestPings = nPings;
	    // cout <<"\t\t Choosing this as the best trackPing"<<endl;
	    theBestPing->mcTrack = (*it).first;
	    // cout <<"\t McTrack: "<<theBestPing->mcTrack<<" nPings Tpc: "<<theBestPing->nPingsTpc<<endl;
	}
    }

    // cout <<"\tChose the best ping.  McTrack: "
    // <<theBestPing->mcTrack<<" nTpcPings: "<<theBestPing->nPingsTpc<<endl;

    return theBestPing;    
}

StTrackPairInfo* StiTrackAssociator::findBestTrackPair(trackPing* ping)
{
    StMcTrack* mcTrack = ping->mcTrack;
    // cout <<"findBestTrackPair(trackPing*).  Looking for McTrack: "<<mcTrack<<endl;
    
    mcTrackMapType* mcToStTrackMap = mAssociationMaker->mcTrackMap();
    if (!mcToStTrackMap) {
	cout <<"StiTrackAssociator::findBestTrackPair(StMcTrack*).  ERROR:\t";
	cout <<"McTrackMap==0"<<endl;
	return 0;
    }
    
    pair<mcTrackMapType::iterator, mcTrackMapType::iterator> range =
	mcToStTrackMap->equal_range(mcTrack);
    if (range.first==range.second) {
	cout <<"Error, range.first==range.second"<<endl;
	return 0;
    }
    
    //Start kludge here (should be an algorithm call, couldn't get solaris to take it!
    StTrackPairInfo* bestPair=0;    
    unsigned int mostCommon=0;
    
    for (mcTrackMapType::iterator it=range.first; it!=range.second; ++it) {
	StTrackPairInfo* info = (*it).second;
	if (info->commonTpcHits()>mostCommon) { //update, remember
	    mostCommon = info->commonTpcHits();
	    bestPair = info;
	}
    }
    //End kludge here;

    return bestPair;
}

void StiTrackAssociator::associate(const StTpcHit* hit)
{
    typedef pair<rcTpcHitMapIter, rcTpcHitMapIter> rcTpcHitMapIterPair;
    
    rcTpcHitMapType* rcTpcHitMap = mAssociationMaker->rcTpcHitMap();
    rcTpcHitMapIterPair thePair = rcTpcHitMap->equal_range(hit);

    //cout <<"\tassociate(StTpcHit*)";
    
    for (rcTpcHitMapIter it=thePair.first; it!=thePair.second; ++it) {
	StMcTrack* mcTrack = (*it).second->parentTrack();
	//cout <<"\tTouching mcTrack: "<<mcTrack;
	++(mMap[mcTrack]).nPingsTpc;
    }
    //cout <<endl;
}

void StiTrackAssociator::associate(const StSvtHit* hit)
{
    typedef pair<rcSvtHitMapIter, rcSvtHitMapIter> rcSvtHitMapIterPair;
    
    rcSvtHitMapType* rcSvtHitMap = mAssociationMaker->rcSvtHitMap();
    rcSvtHitMapIterPair thePair = rcSvtHitMap->equal_range(hit);

    //cout <<"\tassociate(StSvtHit*)";

    //if (thePair.first!=thePair.second) {
    //cout <<"turned up svt hits"<<endl;
    //}
    for (rcSvtHitMapIter it=thePair.first; it!=thePair.second; ++it) {
	StMcTrack* mcTrack = (*it).second->parentTrack();
	//cout <<"\tTouching mcTrack: "<<mcTrack;
	++(mMap[mcTrack]).nPingsSvt;
    }
}

void StiTrackAssociator::associate(const StiHit* hit)
{
    const StHit* stHit = hit->stHit();
    if (const StTpcHit* tpcHit = dynamic_cast<const StTpcHit*>(stHit) ) {
	//retrieve from tpc association
	associate(tpcHit);
    }
    else if (const StSvtHit* svtHit = dynamic_cast<const StSvtHit*>(stHit) ) {
	//retrive from svt association
	associate(svtHit);
    }
    else {
	cout <<"StiTrackAssociator::associate(StiHit*). ERROR:\t"
	     <<"Unknown hit Type.  No action taken"<<endl;
	return;
    }
}
