// -*- mode: c++;-*-
// $Id: StjTrackListCut.h,v 1.1 2008/11/27 07:09:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKLISTCUT_H
#define STJTRACKLISTCUT_H

#include <TObject.h>

#include "StjTrackList.h"
#include "StjTrackCut.h"

#include <vector>

class StjTrackListCut : public TObject {

public:

  StjTrackListCut() { }
  virtual ~StjTrackListCut() { }

  StjTrackList operator()(const StjTrackList& trackList);

  void addCut(StjTrackCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjTrackCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shoudNotPass(const StjTrack& track);

  CutList _cutList;

  ClassDef(StjTrackListCut, 1)

};

#endif // STJTRACKLISTCUT_H
