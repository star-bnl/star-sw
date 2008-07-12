// $Id: StJetTPCTrackCut.cxx,v 1.2 2008/07/12 01:32:07 tai Exp $
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

    TVector3 p(track.px, track.py, track.pz);

    if (_use2006Cuts){
      if(p.Pt() < 0.5) {
	if(track.Tdca > 2.0) return true;
      } else if(p.Pt() < 1.0) {
	if(track.Tdca > 3.-2.*p.Pt()) return true;
      } else {
	if(track.Tdca > 1.0) return true;
      }
    }

    if(p.Eta() < -2.0)
      return true;

    if(p.Eta() > 2.0)
      return true;

    if(static_cast<double>(track.nHits)/static_cast<double>(track.nHitsPoss) < .51)
      return true;

  return false;
}



}
