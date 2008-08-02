// -*- mode: c++;-*-
// $Id: StjTrackListCut.h,v 1.2 2008/08/02 19:22:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKCUT_H
#define STJETTPCTRACKCUT_H

#include "StjTrackList.h"
#include "StjTrackCut.h"

#include <vector>

namespace StSpinJet {

class StjTrackListCut {

public:

  StjTrackListCut() { }
  virtual ~StjTrackListCut() { }

  StjTrackList operator()(const StjTrackList& trackList);

  void addCut(StJetTrackCut::StjTrackCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetTrackCut::StjTrackCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shoudNotPass(const StjTrack& track);

  CutList _cutList;

};

}

#endif // STJETTPCTRACKCUT_H
