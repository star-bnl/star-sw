// $Id: CollectChargedTracksFromTPC.cxx,v 1.10 2008/07/09 23:53:28 tai Exp $
#include "CollectChargedTracksFromTPC.h"

#include "StJetTPC.h"

#include "StJetTPCTrackCut.h"

namespace StSpinJet {

CollectChargedTracksFromTPC::CollectChargedTracksFromTPC(StJetTPC* tpc, StJetTPCTrackCut* cut)
  : _tpc(tpc)
  , _cut(*cut)
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
