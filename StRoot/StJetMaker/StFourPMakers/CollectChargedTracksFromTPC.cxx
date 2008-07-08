// $Id: CollectChargedTracksFromTPC.cxx,v 1.8 2008/07/08 04:56:11 tai Exp $
#include "CollectChargedTracksFromTPC.h"

#include "StJetTPC.h"

#include "StJetTPCTrackCut.h"

namespace StSpinJet {

CollectChargedTracksFromTPC::CollectChargedTracksFromTPC(StJetTPC* tpc)
  : _tpc(tpc)
  , _cut(*(new StJetTPCTrackCut))
{

}

CollectChargedTracksFromTPC::~CollectChargedTracksFromTPC()
{

}

CollectChargedTracksFromTPC::TrackList CollectChargedTracksFromTPC::Do()
{
  TrackList trackList = _tpc->getTrackList();

  trackList = _cut(trackList);

  return trackList;
}

void CollectChargedTracksFromTPC::setUse2006Cuts(bool v)
{ 
  _cut.setUse2006Cuts(v);
}

}
