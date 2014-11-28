//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky 
// 6 March 2012
//

#include "StjTrackPtFraction.h"

ClassImp(StjTrackPtFraction);

StjTrackList StjTrackPtFraction::Do( const StjTrackList& trackList)
{
  StjTrackList elist;
  // Track loop
  for (StjTrackList::const_iterator iTrack = trackList.begin(); iTrack != trackList.end(); ++iTrack) {
    StjTrack track = *iTrack;
    // Add a certain fraction to the track pT
    track.pt += mFraction *track.pt;
    elist.push_back(track);
  } // End track loop  
  return elist;
}
