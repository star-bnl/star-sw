//StiEvaluableTrackSeedFinder.cxx
//M.L. Miller (Yale Software)
//04/01

//Std
#include <iostream>
#include <math.h>
#include <map>

//StEvent
#include "StEventTypes.h"

//StMcEvent
#include "StMcEventTypes.hh"

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
	    go=false;	    //mtrackpair->second = track;
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

    //ATTENTION CLAUDE: Uncomment the following to seed KalmanTrack and investigate problems!
    this->operator()(sttrack, track);
    
    return track;
}

void StiEvaluableTrackSeedFinder::operator() (const StTrack* st, StiKalmanTrack* sti)
{
    //cout <<"StiEvaluableTrackSeedFinder::operator()(StTrack*, StiKalmanTrack*, StiDetector*)"<<endl;
    //now get hits
    StPtrVecHit hits = st->detectorInfo()->hits();
    sort( hits.begin(), hits.end(), StHitRadiusGreaterThan() );
    //sort( hits.begin(), hits.end(), StHitRadiusLessThan() );
    hitvector hitvec;
    
    for (vector<StHit*>::iterator it=hits.begin(); it!=hits.end(); ++it) {
	StTpcHit* hit = dynamic_cast<StTpcHit*>(*it);
	if (!hit) {
	    cout <<"Error: cast failed"<<endl;
	    sti=0;
	    return;
	}
	else {
	    //Find StiHit for this StHit
	    pair<double, double> myPair = StiGeometryTransform::instance()->angleAndPosition(hit);
	    double refAngle=myPair.first;
	    double position=myPair.second;
	    
	    const hitvector& stiHits = StiHitContainer::instance()->hits(refAngle, position);
	    if (stiHits.size()==0) {
		cout <<"Error, no StiHits for this sector, padrow"<<endl;
		sti=0;
		return;
	    }
	    
	    SameStHit mySameStHit;
	    mySameStHit.stHit = hit;
	    hitvector::const_iterator where = find_if(stiHits.begin(), stiHits.end(), mySameStHit);
	    if (where==stiHits.end()) {
		cout <<"Error, no StiHit with this StHit was found"<<endl;
		sti=0;
		return;
	    }
	    else {
		hitvec.push_back(*where);
	    }
	}
    }
    
    //cout <<"Filled Hits: "<<endl;
    //for (hitvector::const_iterator it=hitvec.begin(); it!=hitvec.end(); ++it) {
    //cout <<(*(*it))<<endl;
    //}
    
    //Now get the helix
    StPhysicalHelixD sthelix = st->geometry()->helix();
      
    //Establish the origin in TpcCoordinates
    const StThreeVectorD& stHelixOriginD = sthelix.origin();
    
    double curvature = sthelix.curvature();
    if (sthelix.h()<0) {
	curvature=curvature*-1.;
    }
    //cout <<"curvature: "<<curvature<<endl;
    
    double tanLambda = tan(sthelix.dipAngle());
    //cout <<"tanLambda: "<<tanLambda<<endl;

    sti->initialize(curvature, tanLambda, stHelixOriginD, hitvec);

    //Test track!
    cout <<"Test the track:"<<endl;
    for (double xLocal=hitvec.back()->x(); xLocal<=hitvec.front()->x(); xLocal+=10.) {
	//for (double xLocal=hitvec.front()->x(); xLocal<=hitvec.back()->x(); xLocal+=10.) {
	StThreeVector<double> pos = sti->getGlobalPointNear(xLocal);
	cout <<"\tx: "<<xLocal<<"\tpos: "<<pos<<endl;
    }
    
}
