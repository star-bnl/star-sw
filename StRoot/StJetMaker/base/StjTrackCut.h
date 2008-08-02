// -*- mode: c++;-*-
// $Id: StjTrackCut.h,v 1.2 2008/08/02 19:22:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUT_H
#define TRACKCUT_H

namespace StJetTrackCut {

class StjTrackCut {

public:
  StjTrackCut() { }
  virtual ~StjTrackCut() { }

  virtual bool operator()(const StSpinJet::StjTrack& track) = 0;

};

}

#endif // TRACKCUT_H
