// $Id: StjTrackListCut.cxx,v 1.1 2008/11/27 07:09:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrackListCut.h"

ClassImp(StjTrackListCut)

StjTrackList StjTrackListCut::operator()(const StjTrackList& trackList)
{
  StjTrackList ret;

  for(StjTrackList::const_iterator it = trackList.begin(); it != trackList.end(); ++it) {

    if (shoudNotPass(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}

bool StjTrackListCut::shoudNotPass(const StjTrack& track)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(track)) return true;
  }

  return false;
}
