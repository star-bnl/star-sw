// $Id: StJetTPCTrackCut.cxx,v 1.1 2008/07/08 04:56:12 tai Exp $
#include "StJetTPCTrackCut.h"

#include "../StMuTrackEmu.h"

#include <TVector3.h>

namespace StSpinJet {

StJetTPCTrackCut::TrackList StJetTPCTrackCut::operator()(const TrackList& trackList)
{
  TrackList ret;

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    StMuTrackEmu* track = *it;

    if (shoudNotPass(*track)) continue;

    ret.push_back(track);
  }

  return ret;
}

bool StJetTPCTrackCut::shoudNotPass(const StMuTrackEmu& track) const
{
    if (track.Tdca() > 3.)
      return true;

    TVector3 p(track.px(), track.py(), track.pz());

    if (_use2006Cuts){
      if(p.Pt() < 0.5) {
	if(track.Tdca() > 2.0) return true;
      } else if(p.Pt() < 1.0) {
	if(track.Tdca() > 3.-2.*p.Pt()) return true;
      } else {
	if(track.Tdca() > 1.0) return true;
      }
    }

    if(p.Eta() < -2.0)
      return true;

    if(p.Eta() > 2.0)
      return true;

    if(static_cast<double>(track.nHits())/static_cast<double>(track.nHitsPoss()) < .51)
      return true;

  return false;
}



}
