#ifndef FWD_FWD_DATA_SOURCE_H
#define FWD_FWD_DATA_SOURCE_H

#include <map>
#include "StFwdTrackMaker/include/Tracker/FwdHit.h"


/* Authoritative Data Source for Fwd tracks + hits
 *
 * Separation here is not great, but the track/hit loading 
 * is tightly bound to the StMaker through the ttree and histogram
 * creation, as well as through the Datasets (StEvent, geant )
 * 
 * So while the filling is done elsewhere, this holds that 
 * data and releases the pointers when needed.
 */
class FwdDataSource {
  public:

    using HitMap_t = std::map<int, std::vector<KiTrack::IHit*>>;
    using McTrackMap_t = std::map<int, shared_ptr<McTrack>>;

    HitMap_t &getFttHits( ) {
        return mFttHits;
    };
    HitMap_t &getFstHits() {
        return mFstHits;
    };
    McTrackMap_t &getMcTracks() {
        return mMcTracks;
    };

    // Cleanup
    void clear() {
        // Just empty our vectors, we dont own the memory
        mFttHits.clear();
        mFstHits.clear();
        // the tracks are shared pointers, so they will be taken care of by clearing the map (below)
        mMcTracks.clear();
    }

    // TODO, protect and add interaface for pushing hits / tracks
    HitMap_t mFttHits;
    HitMap_t mFstHits;
    McTrackMap_t mMcTracks;
};




#endif
