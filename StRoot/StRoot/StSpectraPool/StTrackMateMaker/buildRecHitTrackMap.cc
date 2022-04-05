//
//
//
#include <map>
#include "StContainers.h"
#include "StTrackNode.h"
#include "StTrack.h"
#include "StTrackDetectorInfo.h"
#include "StHit.h"

size_t buildRecHitTrackMap(const StSPtrVecTrackNode& nodes,map<StHit*,StTrack*>& htMap){
    size_t failedInserts = 0;
    for (size_t it = 0; it<nodes.size(); ++it) {
	StTrack* track = nodes[it]->track(global);
	if (! track) continue;
	if (track->flag() <= 0) continue;
	if (! track->detectorInfo()) continue;
	StPtrVecHit hits = track->detectorInfo()->hits(kTpcId);
	for (StPtrVecHitIterator hIterTrk = hits.begin(); hIterTrk != hits.end(); ++hIterTrk) {
	    StHit* hit = *hIterTrk;
	    pair<map<StHit*,StTrack*>::iterator,bool> insRes = htMap.insert(map<StHit*,StTrack*>::value_type(hit,track));
	    if (insRes.second==false) ++failedInserts;	    
	}//hits in track loop
    }// track loop
    return failedInserts;
}
