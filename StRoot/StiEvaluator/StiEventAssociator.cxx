//StiEventAssociator.cxx
//M.L. Miller (Yale Software)
//12/01

//std
#include <iostream>
#include <math.h>
#include <algorithm>
#include <map>
using namespace std;

//StMcEvent
#include "StMcEventTypes.hh"

//Sti
#include "Sti/StiTrackContainer.h"
#include "Sti/StiKalmanTrack.h"

//StiEvaluator
#include "StiTrackAssociator.h"
#include "StiEventAssociator.h"

StiEventAssociator* StiEventAssociator::sInstance = 0;

StiEventAssociator* StiEventAssociator::instance(StAssociationMaker* a)
{
    return (sInstance) ? sInstance : new StiEventAssociator(a);
}

StiEventAssociator::StiEventAssociator(StAssociationMaker* a)
    : mAssociationMaker(a), mTrackAssociator(new StiTrackAssociator(a)),
      mTrackStore(StiTrackContainer::instance()), mMcEvent(0)
{
    cout <<"StiEventAssociator::StiEventAssociator()"<<endl;
    if (!mAssociationMaker) {
	cout <<"StiEventAssociator::StiEventAssociatorr(). ERROR:\t"
	     <<"mAssociationMaker null.  Undefined behavior"<<endl;
    }
    sInstance = this;
}

StiEventAssociator::~StiEventAssociator()
{
    cout <<"StiEventAssociator::StiEventAssociator()"<<endl;
    delete mTrackAssociator;
    mTrackAssociator = 0;
}

void StiEventAssociator::clear()
{
    mMcKeyMap.clear();
    mStiKeyMap.clear();
}

void StiEventAssociator::associate(StMcEvent* mc)
{
    cout <<"StiEventAssociator::associate()"<<endl;
    
    mMcEvent = mc;
    clear();

    //loopOnFoundTracks
    cout <<"\tLooping on found tracks"<<endl;
    for (stitrackvec::iterator it=mTrackStore->begin(); it!=mTrackStore->end(); ++it) {
	StiKalmanTrack* track = dynamic_cast<StiKalmanTrack*>(*it);
	if (track) {
	    fillForFoundTrack(track);
	}
    }
    
    //loopOnMcTracks
    cout <<"\tLooping on McTracks"<<endl;
    StSPtrVecMcTrack& tracks = mMcEvent->tracks();
    for (vector<StMcTrack*>::iterator it=tracks.begin(); it!=tracks.end(); ++it) {
	fillForMcTrack(*it);
    }
    cout <<"Done Associating for event"<<endl;
}

void StiEventAssociator::fillForMcTrack(StMcTrack* mcTrack)
{
    //Check to see if it was found:
    McToInfoPairMap::const_iterator it = mMcKeyMap.find(mcTrack);
    if (it!=mMcKeyMap.end()) { //This McTrack was found
	return;
    }

    //Else, fill with an empty pair to signal that it wasn't found!
    InfoPair myInfoPair( 0, StiTrackPairInfo() );
    mMcKeyMap.insert( McToInfoPairMap_ValType( mcTrack, myInfoPair) );

    //That's it!
}

void StiEventAssociator::fillForFoundTrack(StiKalmanTrack* track)
{
    StiTrackAssociator::AssocPair asPair = mTrackAssociator->associate(track);	

    //note: newInfo can be 0 if ITTF assoc worked and global failed.
    //We don't consider the opposite case, since we're not worried about evaluating Tpt
    StTrackPairInfo* newInfo = asPair.first;
    const trackPing& ping = asPair.second;

    if (ping.mcTrack==0) {
	return;
    }
    StMcTrack* mcTrack = ping.mcTrack;

    InfoPair myInfoPair( newInfo, StiTrackPairInfo(track, ping) );

    //First fill for the single Ittf -> Mc assoc.
    //It's a map, not a multi-map, so we can safely do easy insert
    mStiKeyMap[track] = myInfoPair;

    //Now fill for the mutlipe Mc -> ITTF assoc
    mMcKeyMap.insert( McToInfoPairMap_ValType( mcTrack, myInfoPair) );
    
}
