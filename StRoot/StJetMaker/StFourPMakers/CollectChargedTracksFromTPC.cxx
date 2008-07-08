// $Id: CollectChargedTracksFromTPC.cxx,v 1.4 2008/07/08 02:26:40 tai Exp $
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
  TrackList__ trackList = getTracksFromTPC();

  vector<StMuTrackEmu*> trackmuList;

  double magneticField = _uDstMaker->muDst()->event()->magneticField()/10.0; //to put it in Tesla
  for(TrackList__::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    const StMuTrack* track = (*it).first;

    StMuTrackEmu* trackEmu = StMuTrackEmuFactory::createStMuTrackEmu(track, (*it).second, magneticField);

    trackmuList.push_back(trackEmu);
  }

  trackmuList = selectTracksToPassToJetFinder(trackmuList);


  return trackmuList;
}

CollectChargedTracksFromTPC::TrackList__ CollectChargedTracksFromTPC::getTracksFromTPC()
{
  TrackList__ ret;

  StMuDst* uDst = _uDstMaker->muDst();

  long nTracks = uDst->numberOfPrimaryTracks();

  for(int i = 0; i < nTracks; ++i) {
    const StMuTrack* track = uDst->primaryTracks(i);

    if(track->flag() < 0) continue;

    if(track->topologyMap().trackFtpcEast() || track->topologyMap().trackFtpcWest()) continue;

    ret.push_back(make_pair(track, i));
  }

  return ret;
}

CollectChargedTracksFromTPC::TrackList__ CollectChargedTracksFromTPC::selectTracksToPassToJetFinder(const TrackList__& trackList)
{
  TrackList__ ret;

  for(TrackList__::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {
    const StMuTrack* track = (*it).first;

    if (shoudNotPassToJetFinder(*track)) continue;

    ret.push_back(make_pair((*it).first, (*it).second));
  }

  return ret;
}

bool CollectChargedTracksFromTPC::shoudNotPassToJetFinder(const StMuTrack& track) const
{
    if (track.dcaGlobal().mag() > 3.)
      return true;
      
    if (_use2006Cuts){
      if(track.pt() < 0.5) {
	if(track.dcaGlobal().mag() > 2.0) return true;
      } else if(track.pt() < 1.0) {
	if(track.dcaGlobal().mag() > 3.-2.*track.pt()) return true;
      } else {
	if(track.dcaGlobal().mag() > 1.0) return true;
      }
    }

    if(track.eta() < -2.0)
      return true;

    if(track.eta() > 2.0)
      return true;

    if(static_cast<double>(track.nHits())/static_cast<double>(track.nHitsPoss()) < .51)
      return true;

  return false;
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
