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
#include "StiPlacement.h"
#include "StiDetector.h"
#include "StiDetectorContainer.h"
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

    //ATTENTION CLAUDE: Uncomment the following to seed KalmanTrack and investigate problems!
    /*
      const StiDetectorContainer& rdet = *(StiDetectorContainer::instance());
      StiDetector* layer = *rdet;
      this->operator()(sttrack, track, layer);
    */
    
    return track;
}

void StiEvaluableTrackSeedFinder::operator() (const StTrack* st, StiKalmanTrack* sti, const StiDetector* layer)
{
    //cout <<"StiEvaluableTrackSeedFinder::operator()(StTrack*, StiKalmanTrack*, StiDetector*)"<<endl;
    //Set x and refAngle
    double x = layer->getPlacement()->getCenterRadius();
    double refAngle = layer->getPlacement()->getCenterRefAngle();
    //cout <<"x: "<<x<<"\trefAngle: "<<refAngle<<endl;
    
    //Now get the helix
    StPhysicalHelixD sthelix = st->geometry()->helix();
    
    //Establish the origin in TpcCoordinates
    const StThreeVectorD& stHelixoriginD = sthelix.origin();
    StThreeVectorD originD( sthelix.xcenter(), sthelix.ycenter(), stHelixoriginD.z() );
    //cout <<"originD: "<<originD<<endl;
    
    //Transform origin to StiCoordinates
    StThreeVectorD stioriginD = StiGeometryTransform::instance()->operator()(originD, refAngle);
    //cout <<"stioriginD: "<<stioriginD<<endl;
    
    double curvature = sthelix.curvature();
    //cout <<"curvature: "<<curvature<<endl;
    //double curvature = sthelix.curvature()*static_cast<double>(sthelix.h());
    double tanLambda = tan(sthelix.dipAngle());
    //cout <<"tanLambda: "<<tanLambda<<endl;
    double eta = curvature*originD.x();
    //cout <<"eta: "<<eta<<endl;
    double y = originD.y() - 1./curvature*sqrt(1.- (curvature*x*eta)*(curvature*x*eta));
    //cout <<"y: "<<y<<endl;
    double z = originD.z() - tanLambda/curvature*asin(curvature*x-eta);
    //cout <<"z: "<<z<<endl;
    double state[5];
    state[0] = y;
    state[1] = z;
    state[2] = eta;
    state[3] = curvature;
    state[4] = tanLambda;

    //cout <<"Set State"<<endl;
    double dstate[15];
    for (int i=0; i<15; ++i) {dstate[i]=1.;} //Dummy errors, all the same
    //cout <<"Set Errors"<<endl;
    
    //now get hits
    StPtrVecHit hits = st->detectorInfo()->hits();
    sort( hits.begin(), hits.end(), StHitRadiusGreaterThan() );
    //cout <<"Sorted Hits"<<endl;
    hitvector hitvec;
    
    for (vector<StHit*>::iterator it=hits.begin(); it!=hits.end(); ++it) {
	//Transform to StiHit
	//StiHit* temp = new StiHit(); //This is a total memory leak, only temporary
	StiHit* temp = mhitfactory->getObject();
	StTpcHit* hit = dynamic_cast<StTpcHit*>(*it);
	if (!hit) {
	    cout <<"Error: cast failed"<<endl;
	}
	else {
	    StiGeometryTransform::instance()->operator()(hit, temp);
	    hitvec.push_back(temp);
	}
    }
    
    //cout <<"Filled Hits: "<<endl;
    //for (hitvector::const_iterator it=hitvec.begin(); it!=hitvec.end(); ++it) {
    //cout <<(*(*it))<<endl;
    //}
    //cout <<"Passing State"<<endl;
    //for (int i=0; i<5; ++i) {
    //cout <<"state["<<i<<"]:\t"<<state[i]<<endl;
    //}
    //for (int i=0; i<15; ++i) {
    //cout <<"dstate["<<i<<"]:\t"<<dstate[i]<<endl;
    //}
    
    //That's it, seed the track and go home
    sti->initialize(refAngle, state, dstate, hitvec);
    //cout <<"Initialized StiKalmanTrack"<<endl;
    
}
