// -*- mode: c++;-*-
// $Id: StjTrackListCut.h,v 1.3 2008/08/02 22:43:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKLISTCUT_H
#define STJTRACKLISTCUT_H

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

#endif // STJTRACKLISTCUT_H
