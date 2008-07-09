// $Id: CollectChargedTracksFromTPC.cxx,v 1.9 2008/07/09 02:40:04 tai Exp $
#include "CollectChargedTracksFromTPC.h"

#include "StJetTPC.h"

#include "StJetTPCTrackCut.h"

#include "StJetTPCTrackPrint.h"

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

  //  StJetTPCTrackPrint print;
  //  print(trackList);

  trackList = _cut(trackList);

  return trackList;
}

void CollectChargedTracksFromTPC::setUse2006Cuts(bool v)
{ 
  _cut.setUse2006Cuts(v);
}

}
