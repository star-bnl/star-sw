//StiEvaluableTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//04/01

//Std
#include <iostream>
#include <math.h>

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

//Sti
#include "StiEvaluableTrack.h"
#include "StiStTrackFilter.h"
#include "StiGeometryTransform.h"
#include "StiEvaluableTrackSeedFinder.h"

StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder() : mevent(0), mmcevent(0)
{
    cout <<"StiEvaluableTrackSeedFinder::StiEvaluableTrackSeedFinder()"<<endl;
}

StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()
{
    cout <<"StiEvaluableTrackSeedFinder::~StiEvaluableTrackSeedFinder()"<<endl;
    mevent = 0;
    mmcevent = 0;
}

void StiEvaluableTrackSeedFinder::setEvent(StEvent* evt, StMcEvent* mcevt) 
{
    mevent = evt;
    mmcevent = mcevt;

    //Get StTracks from StEvent
    StSPtrVecTrackNode& trackNode = mevent->trackNodes();
    mbegin = trackNode.begin();
    mend = trackNode.end();
    mcurrent = trackNode.begin();

    if (mcevt!=0) {
	//Get StMcTrack from StMcEvent
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
    return (mcurrent!=mend);
}

StiKalmanTrack* StiEvaluableTrackSeedFinder::next()
{
    bool go=true;
    StTrack* track = 0;
    StiEvaluableTrack* returntrack = 0;

    while (go && mcurrent!=mend) {
	track = (*mcurrent)->track(mtype);
	if (track && track->flag()>=0) {
	    go=false;
	    //mtrackpair->second = track;
	    ++mcurrent;
	    returntrack = makeTrack(track);
	}
	else ++mcurrent;
    }
    return returntrack;
}

StiEvaluableTrack* StiEvaluableTrackSeedFinder::makeTrack(StTrack* sttrack)
{
    StiEvaluableTrack* track = mfactory->getObject();
    track->reset();
    track->setStTrack(sttrack);
    StiGeometryTransform::instance()->operator()(sttrack, track);
    
    return track;
}
