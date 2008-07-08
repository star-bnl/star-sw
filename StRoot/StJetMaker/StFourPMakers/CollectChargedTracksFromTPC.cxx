// $Id: CollectChargedTracksFromTPC.cxx,v 1.5 2008/07/08 02:40:30 tai Exp $
#include "CollectChargedTracksFromTPC.h"

#include "StMuDSTMaker/COMMON/StMuTrack.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "../StMuTrackEmu.h"
#include "../StMuTrackEmuFactory.h"

#include <TVector3.h>

namespace StSpinJet {

CollectChargedTracksFromTPC::CollectChargedTracksFromTPC(StMuDstMaker* uDstMaker)
  : _uDstMaker(uDstMaker)
  , _use2006Cuts(false)
{

}

CollectChargedTracksFromTPC::~CollectChargedTracksFromTPC()
{

}

CollectChargedTracksFromTPC::TrackList CollectChargedTracksFromTPC::Do()
{
  TrackList trackList = getTracksFromTPC();

  trackList = selectTracksToPassToJetFinder(trackList);

  return trackList;
}

CollectChargedTracksFromTPC::TrackList CollectChargedTracksFromTPC::getTracksFromTPC()
{
  TrackList ret;

  StMuDst* uDst = _uDstMaker->muDst();

  long nTracks = uDst->numberOfPrimaryTracks();

  double magneticField = _uDstMaker->muDst()->event()->magneticField()/10.0; // Tesla
  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* track = uDst->primaryTracks(i);

    if(track->flag() < 0) continue;

    if(track->topologyMap().trackFtpcEast() || track->topologyMap().trackFtpcWest()) continue;

    StMuTrackEmu* trackEmu = StMuTrackEmuFactory::createStMuTrackEmu(track, i, magneticField);

    ret.push_back(trackEmu);
  }

  return ret;
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
