//
// $Id: dominatrackInfo.cc,v 1.4 2018/01/03 18:18:10 genevb Exp $
//
// $Log: dominatrackInfo.cc,v $
// Revision 1.4  2018/01/03 18:18:10  genevb
// idTruths and keys moved from short to int
//
// Revision 1.3  2007/02/23 17:07:41  fisyak
// Resolve bug #682
//
// Revision 1.2  2005/07/19 22:05:19  perev
// MultiVertex
//
// Revision 1.1  2004/03/31 23:44:36  calderon
// Function to find the dominatrack, the number of hits belonging to the
// dominatrack and the average hit quality of those hits (based on idTruth and
// quality of StHit).
//
//
#include <map>
#include <set>
#include "Stiostream.h"
#include "StTrack.h"
#include "StContainers.h"
#include "StTrackDetectorInfo.h"
#include "StEnumerations.h"
#include "StHit.h"

void dominatrackInfo(const StTrack* recTrack,int& dominatrackKey ,short& dominatrackHits,float& avgQuality) {
    // initialize return values.
    // dominatrack key initialized to nonsense, quality initialized to 0.
    // Note, I'm using shorts, which should be ok up to 32768, but if we
    // ever have track keys above this in an event, there will be trouble. 
  static short DetectorList[kMaxDetectorId];
    dominatrackKey = -999;
    dominatrackHits = 0; 
    avgQuality = 0;
    multimap<int,float> idTruths;
    set<int> uniqueIdTruths;
    if (!recTrack) return;
    StPtrVecHit recHits = recTrack->detectorInfo()->hits();//(kTpcId);
    // loop to store all the mc track keys and quality of every reco hit on the track.
    for (StHitIterator hi=recHits.begin();
	 hi!=recHits.end(); hi++) {
	StHit* rHit = *hi; 
	idTruths.insert( multimap<int,float>::value_type(rHit->idTruth(),rHit->qaTruth()));
	uniqueIdTruths.insert(static_cast<int>(rHit->idTruth()));
    }
    // find the dominatrix track!
    for (set<int>::iterator si=uniqueIdTruths.begin(); si!=uniqueIdTruths.end(); ++si) {
	int currentNHitsIdTruth = idTruths.count(*si);
	if (currentNHitsIdTruth>dominatrackHits) {
	    dominatrackKey = *si; 
	    dominatrackHits = currentNHitsIdTruth;
	}
    }
    //calculate average track quality for the dominatrix track
    pair<multimap<int,float>::iterator,multimap<int,float>::iterator> dominatrackRange = idTruths.equal_range(dominatrackKey);
    for (multimap<int,float>::iterator mi=dominatrackRange.first; mi!=dominatrackRange.second; ++mi) {
	avgQuality+=mi->second;
    }
    avgQuality/=dominatrackHits;
    memset (DetectorList, 0, kMaxDetectorId*sizeof(short));
    for (StHitIterator hi=recHits.begin();
	 hi!=recHits.end(); hi++) {
      StHit* rHit = *hi;
      if (rHit->idTruth() == dominatrackKey) {
	DetectorList[rHit->detector()]++;
      } 
#if 0
      else 	cout << "Not Matched hit " << *rHit << endl;
#endif
    }
    if (DetectorList[kTpcId] > 99) DetectorList[kTpcId] = 99;
    if (DetectorList[kSvtId] >  9) DetectorList[kSvtId] =  9;
    if (DetectorList[kSsdId] >  9) DetectorList[kSsdId] =  9;
    dominatrackHits = DetectorList[kTpcId] + 100*(DetectorList[kSvtId] + 10*DetectorList[kSsdId]);
    return;
}
