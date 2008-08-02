// -*- mode: c++;-*-
// $Id: StjTrackCut.h,v 1.3 2008/08/02 22:43:21 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUT_H
#define STJTRACKCUT_H

namespace StJetTrackCut {

class StjTrackCut {

public:
  StjTrackCut() { }
  virtual ~StjTrackCut() { }

  virtual bool operator()(const StSpinJet::StjTrack& track) = 0;

};

}

#endif // STJTRACKCUT_H
