// $Id: StJetTPCTrackCut.cxx,v 1.1 2008/07/21 17:24:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTPCTrackCut.h"

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

bool StJetTPCTrackCut::shoudNotPass(const Track& track)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(track)) return true;
  }

  return false;
}



}
