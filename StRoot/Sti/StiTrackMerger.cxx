//StiTrackMerger.cxx
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

#include "StiTrackMerger.h"

StiTrackMerger::StiTrackMerger(StiTrackContainer* store)
    : mTrackStore(store), mSubject(StiIOBroker::instance())
{
    cout <<"StiTrackMerger::StiTrackMerger()"<<endl;
    mSubject->attach(this);
}

StiTrackMerger::~StiTrackMerger()
{
    cout <<"StiTrackMerger::~StiTrackMerger()"<<endl;
    if (mSubject) {
	mSubject->detach(this);
    }
}
