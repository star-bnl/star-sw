// -*- mode: c++;-*-
// $Id: StjTrackCut.h,v 1.4 2008/08/03 00:26:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUT_H
#define STJTRACKCUT_H

class StjTrackCut {

public:
  StjTrackCut() { }
  virtual ~StjTrackCut() { }

  virtual bool operator()(const StjTrack& track) = 0;

};

#endif // STJTRACKCUT_H
