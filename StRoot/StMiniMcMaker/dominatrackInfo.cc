//
// $Id: dominatrackInfo.cc,v 1.1 2004/03/31 23:44:36 calderon Exp $
//
// $Log: dominatrackInfo.cc,v $
// Revision 1.1  2004/03/31 23:44:36  calderon
// Function to find the dominatrack, the number of hits belonging to the
// dominatrack and the average hit quality of those hits (based on idTruth and
// quality of StHit).
//
//
#include <map>
#include <set>
#include "StTrack.h"
#include "StContainers.h"
#include "StTrackDetectorInfo.h"
#include "StEnumerations.h"
#include "StHit.h"

void dominatrackInfo(const StTrack* recTrack,short&dominatrackKey ,short& dominatrackHits,float& avgQuality) {
    // initialize return values.
    // dominatrack key initialized to nonsense, quality initialized to 0.
    // Note, I'm using shorts, which should be ok up to 32768, but if we
    // ever have track keys above this in an event, there will be trouble. 
    dominatrackKey = -999;
    dominatrackHits = 0; 
    avgQuality = 0;
    multimap<short,float> idTruths;
    set<short> uniqueIdTruths;
    if (!recTrack) return;
    StPtrVecHit recTpcHits = recTrack->detectorInfo()->hits(kTpcId);
    // loop to store all the mc track keys and quality of every reco hit on the track.
    for (StHitIterator hi=recTpcHits.begin();
	 hi!=recTpcHits.end(); hi++) {
	StHit* rHit = *hi;
	idTruths.insert( multimap<short,float>::value_type(rHit->idTruth(),rHit->quality()));
	uniqueIdTruths.insert(static_cast<int>(rHit->idTruth()));
    }
    // find the dominatrix track!
    for (set<short>::iterator si=uniqueIdTruths.begin(); si!=uniqueIdTruths.end(); ++si) {
	int currentNHitsIdTruth = idTruths.count(*si);
	if (currentNHitsIdTruth>dominatrackHits) {
	    dominatrackKey = *si; 
	    dominatrackHits = currentNHitsIdTruth;
	}
    }
    //calculate average track quality for the dominatrix track
    pair<multimap<short,float>::iterator,multimap<short,float>::iterator> dominatrackRange = idTruths.equal_range(dominatrackKey);
    for (multimap<short,float>::iterator mi=dominatrackRange.first; mi!=dominatrackRange.second; ++mi) {
	avgQuality+=mi->second;
    }
    avgQuality/=dominatrackHits;
    return;
}
