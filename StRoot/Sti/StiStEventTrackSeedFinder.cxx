//StiSteventTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//04/01

//Std
#include <iostream>
#include <math.h>

//StEvent
#include "StEventTypes.h"

//Sti
#include "StiDummyTrack.h"
#include "StiStTrackFilter.h"
#include "StiStEventTrackSeedFinder.h"

StiStEventTrackSeedFinder::StiStEventTrackSeedFinder()
{
    cout <<"StiStEventTrackSeedFinder::StiStEventTrackSeedFinder()"<<endl;
    mevent = 0;
    mtrackpair = new sti_track_pair();
}

StiStEventTrackSeedFinder::~StiStEventTrackSeedFinder()
{
    cout <<"StiStEventTrackSeedFinder::~StiStEventTrackSeedFinder()"<<endl;
    mevent = 0;
    delete mtrackpair;
    mtrackpair = 0;
}

void StiStEventTrackSeedFinder::setEvent(StEvent* evt) 
{
    mevent = evt;
    StSPtrVecTrackNode& trackNode = mevent->trackNodes();
    mbegin = trackNode.begin();
    mend = trackNode.end();
    mcurrent = trackNode.begin();
    return;
}

void StiStEventTrackSeedFinder::addStTrackFilter(StiStTrackFilter* filter)
{
    mfiltervector.push_back(filter);
    return;
}

void StiStEventTrackSeedFinder::setStTrackType(StTrackType thetype)
{
    mtype = thetype;
    return;
}

bool StiStEventTrackSeedFinder::hasMore()
{
    return (mcurrent!=mend);
}

StiSeedFinder::sti_track_pair* StiStEventTrackSeedFinder::next()
{
    mtrackpair->first=0;
    mtrackpair->second=0;
    bool go=true;
    while (go && mcurrent!=mend) {
	StTrack* track = (*mcurrent)->track(mtype);
	if (track && track->flag()>=0) {
	    go=false;
	    mtrackpair->second = track;
	    ++mcurrent;
	}
	else ++mcurrent;
    }
    return mtrackpair;
}
