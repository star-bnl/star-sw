//StiStEventFiller.cxx
//Manuel Calderon (Yale Software)
//3/02

//std
#include <iostream>
#include <algorithm>
using namespace std;

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiTrackContainer.h"
#include "Sti/StiKalmanTrack.h"

//StiMaker
#include "StiStEventFiller.h"

StiStEventFiller::StiStEventFiller() : mEvent(0), mTrackStore(0)
{
    //temp, make sure we're not constructing extra copies...
    cout <<"StiStEventFiller::StiStEventFiller()"<<endl;
}

StiStEventFiller::~StiStEventFiller()
{
    cout <<"StiStEventFiller::~StiStEventFiller()"<<endl;
}

//Helper functor, gotta live some place else, just a temp. test of StiTrack::stHits() method
struct StreamStHit
{
    void operator()(const StHit* h) {
	if (const StTpcHit* hit = dynamic_cast<const StTpcHit*>(h)) {
	    cout <<hit->position()<<"\tSector: "<<hit->sector()<<"\tPadrow: "<<hit->padrow()<<endl;
	}
	else if (const StSvtHit* hit = dynamic_cast<const StSvtHit*>(h)) {
	    cout <<hit->position()<<" layer: "<<hit->layer()<<" ladder: "<<hit->ladder()
		 <<" wafer: "<<hit->wafer()<<" barrel: "<<hit->barrel()<<endl;
	}
	else {	
	    cout <<hit->position()<<endl;
	}
    }
};

/*! Describe the time complexity of the call here.  Describe the algorithm here.
  You can put code snippets here, too, like: <code> \n
  StEvent* event = e; \n
  My code operation on e; \n
  </code>
  In general, we put the detailed description with the implementation of the
  function, and a one-liner with the definition of th function.
*/
StEvent* StiStEventFiller::fillEvent(StEvent* e, StiTrackContainer* t)
{
    cout <<"StiStEventFiller::fillEvent()"<<endl;

    if (e==0 || t==0) {
	cout <<"StiStEventFiller::fillEvent(). ERROR:\t"
	     <<"Null StEvent ("<<e<<") || StiTrackContainer ("<<t<<").  Exit"<<endl;
	return 0;
    }
    
    mEvent = e;
    mTrackStore = t;

    //Compute...
    
    for (KalmanTrackMap::const_iterator it=mTrackStore->begin(); it!=mTrackStore->end(); ++it) {
	const StiKalmanTrack* track = (*it).second;

	//Here's a nice test of the hits...
	//vector<StHit*> vec = track->stHits();
	//cout <<" --- Hits for next track --- "<<endl;
	//for_each(vec.begin(), vec.end(), StreamStHit());
    }

    return mEvent;
}
