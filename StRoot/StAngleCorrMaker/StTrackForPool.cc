#include "StTrackForPool.hh"


void StTrackForPool::setTrackForPool(StThreeVectorD momentum, long event) {

    // need to make a copy of this global track, store the pointer
 
    mMomentum = momentum;
    mEventNumber = event;

  };




