//StiLocalTrackMerger.cxx
//M.L. Miller (Yale Software)
//12/01

//std
#include <iostream>
#include <cmath>
#include <algorithm>
using namespace std;

//Sti
#include "StiKalmanTrack.h"
#include "StiTrackContainer.h"
#include "StiKalmanTrackNode.h"
#include "StiIOBroker.h"

#include "StiLocalTrackMerger.h"

StiLocalTrackMerger::StiLocalTrackMerger(StiTrackContainer* store)
    : StiTrackMerger(store), mDeltaR(0.)
{
    cout <<"StiLocalTrackMerger::StiLocalTrackMerger()"<<endl;
    getNewState();
    mMaxTrack.reset();
    mMaxTrackNode.reset();
}

StiLocalTrackMerger::~StiLocalTrackMerger()
{
    cout <<"StiLocalTrackMerger::~StiLocalTrackMerger()"<<endl;
}

void StiLocalTrackMerger::getNewState()
{
    StiIOBroker* broker = StiIOBroker::instance();

    mDeltaR = broker->ltmDeltaR();
}

void StiLocalTrackMerger::mergeTracks()
{
    cout <<"StiLocalTrackMerger::mergeTracks()"<<endl;
    
    if (!mTrackStore) {
	cout <<"StiLocalTrackMerger::mergeTracks(). ERROR:\t"
	     <<"mTrackStore==0.  Abort"<<endl;
	return;
    }
    
    cout <<"\t N-tracks before merge: "<<mTrackStore->size()<<endl;

    for (TrackMap::iterator lower=mTrackStore->begin(); lower!=mTrackStore->end(); ++lower) {
	if (configureMaxTrack( static_cast<StiKalmanTrack*>((*lower).second) )) {

	    //Now find the bounds of search:
	    TrackMap::iterator upper = mTrackStore->upper_bound(&mMaxTrack);
	    if (lower!=upper) {
		TrackMap::iterator start = lower;
		++start;
		int i=0;
		for (TrackMap::iterator it = start; it!=upper; ++it) {
		    ++i;
		    if (sameTrack( static_cast<StiKalmanTrack*>((*start).second), 
				   static_cast<StiKalmanTrack*>((*it).second) ) ) {
			cout <<"sameTrack==true"<<endl;
		    }
		}
		cout <<"Considered: "<<i<<" pairs for merging"<<endl;
	    }
	}
    }
    cout <<"\t N-tracks before merge: "<<mTrackStore->size()<<endl;
}


bool StiLocalTrackMerger::configureMaxTrack(StiKalmanTrack* lowerTrack)
{
    //cout <<"StiLocalTrackMerger::configureMaxTrack()"<<endl;
    if (!lowerTrack) {
	cout <<"StiLocalTrackMerger::configureMaxTrack(). ERROR:\t"
	     <<"lowerTrack==0.  Skip this track"<<endl;
	return false;
    }
    
    StiKalmanTrackNode* lowerNode = lowerTrack->getLastNode();
    if (lowerNode->fP3==0.) {
	cout <<"StiLocalTrackMerger::configureMaxTrack(). ERROR:\t"
	     <<"lowerNode->fP3==0.  Skip this track"<<endl;
	return false;
    }
    
    mMaxTrack.reset();
    mMaxTrackNode.reset();
    
    //Now find the upper limit for search:
    mMaxTrack.reset();
    mMaxTrackNode.reset();
    
    double radius = 1./lowerNode->fP3;  //Careful, this number is signed
    double maxRadius=0.;
    double dR = radius*mDeltaR;
    if (radius<0) {
	maxRadius = radius-dR;
    }
    else {
	maxRadius = radius+dR;
    }
    cout <<"radius: "<<radius<<" dR: "<<dR<<" maxRadius: "<<maxRadius<<endl;
    
    mMaxTrackNode.fP3 = 1./maxRadius;
    ///////mMaxTrack.setLastNode(&mMaxTrackNode); <<< error
    //StiKalmanTrackNode* temp = mMaxTrack.getLastNode();
    //cout <<"maxRadius: "<<1./temp->fP3<<endl;
    
    return true;
}

bool StiLocalTrackMerger::sameTrack(StiKalmanTrack* lhs, StiKalmanTrack* rhs)
{
    return false;
}
