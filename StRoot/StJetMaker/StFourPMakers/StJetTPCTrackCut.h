// -*- mode: c++;-*-
// $Id: StJetTPCTrackCut.h,v 1.5 2008/07/13 09:37:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKCUT_H
#define STJETTPCTRACKCUT_H

#include "TrackList.h"
#include "TrackCut.h"

#include <vector>

namespace StSpinJet {

class StJetTPCTrackCut {

public:

  StJetTPCTrackCut() { }
  virtual ~StJetTPCTrackCut() { }

  TrackList operator()(const TrackList& trackList);

  void addCut(StJetTrackCut::TrackCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTrackCut::TrackCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shoudNotPass(const Track& track);

  CutList _cutList;

};

}

#endif // STJETTPCTRACKCUT_H
