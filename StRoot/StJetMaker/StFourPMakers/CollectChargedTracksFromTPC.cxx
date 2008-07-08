// $Id: CollectChargedTracksFromTPC.cxx,v 1.7 2008/07/08 04:16:03 tai Exp $
#include "CollectChargedTracksFromTPC.h"

#include "StJetTPC.h"


#include "../StMuTrackEmu.h"

#include <TVector3.h>

namespace StSpinJet {

CollectChargedTracksFromTPC::CollectChargedTracksFromTPC(StJetTPC* tpc)
  : _tpc(tpc)
  , _use2006Cuts(false)
{

}

CollectChargedTracksFromTPC::~CollectChargedTracksFromTPC()
{

}

CollectChargedTracksFromTPC::TrackList CollectChargedTracksFromTPC::Do()
{
  TrackList trackList = _tpc->getTrackList();

  trackList = selectTracksToPassToJetFinder(trackList);

  return trackList;
}


CollectChargedTracksFromTPC::TrackList CollectChargedTracksFromTPC::selectTracksToPassToJetFinder(const TrackList& trackList)
{
  TrackList ret;

  for(TrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    StMuTrackEmu* track = *it;

    if (shoudNotPassToJetFinder(*track)) continue;

    ret.push_back(track);
  }

  return ret;
}

bool CollectChargedTracksFromTPC::shoudNotPassToJetFinder(const StMuTrackEmu& track) const
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
