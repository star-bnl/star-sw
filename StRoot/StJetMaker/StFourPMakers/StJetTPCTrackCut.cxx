// $Id: StJetTPCTrackCut.cxx,v 1.3 2008/07/13 06:04:41 tai Exp $
#include "StJetTPCTrackCut.h"

#include "../StMuTrackEmu.h"

#include <TVector3.h>

namespace StSpinJet {

TrackList StJetTPCTrackCut::operator()(const TrackList& trackList)
{
  TrackList ret;

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {

    if (shoudNotPass(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}

bool StJetTPCTrackCut::shoudNotPass(const Track& track) const
{
    if (track.Tdca > 3.)
      return true;

    if (_use2006Cuts){
      if(track.pt < 0.5) {
	if(track.Tdca > 2.0) return true;
      } else if(track.pt < 1.0) {
	if(track.Tdca > 3.-2.*track.pt) return true;
      } else {
	if(track.Tdca > 1.0) return true;
      }
    }

    if(track.eta < -2.0)
      return true;

    if(track.eta > 2.0)
      return true;

    if(static_cast<double>(track.nHits)/static_cast<double>(track.nHitsPoss) < .51)
      return true;

  return false;
}



}
