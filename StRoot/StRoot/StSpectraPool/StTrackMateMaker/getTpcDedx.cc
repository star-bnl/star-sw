//
// getTpcDedx.cc
//
#include "StTrack.h"
#include "StContainers.h"
#include "StDedxPidTraits.h"

float getTpcDedx(StTrack* trk) {
    const StSPtrVecTrackPidTraits& vec = trk->pidTraits();
    const StDedxPidTraits* mTraits = 0;
    for (unsigned int i=0; i<vec.size(); i++) {
        const StDedxPidTraits *p = dynamic_cast<const StDedxPidTraits*>(vec[i]);
        if (p && p->detector() == kTpcId && p->method() == kTruncatedMeanId) mTraits = p;
    }
    if (!mTraits) return 0;    // no info available
    
    return mTraits->mean();
}
