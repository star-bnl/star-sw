//StiStEventFiller.cxx
//Manuel Calderon (Yale Software)
//3/02

//std
#include <iostream>
using namespace std;

//StEvent
#include "StEventTypes.h"

//Sti
#include "Sti/StiTrackContainer.h"

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
    //temp...
    cout <<"StiStEventFiller::fillEvent()"<<endl;
    if (!mEvent || !t) {
	cout <<"StiStEventFiller::fillEvent(). ERROR:\t"
	     <<"Null StEvent or null StiTrackContainer.  Exit"<<endl;
	return 0;
    }
    mEvent = e;
    mTrackStore = t;

    //Compute...

    return mEvent;
}
