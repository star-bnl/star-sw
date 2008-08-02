// -*- mode: c++;-*-
// $Id: StjTrackCut.h,v 1.1 2008/08/02 04:16:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUT_H
#define TRACKCUT_H

namespace StJetTrackCut {

class TrackCut {

public:
  TrackCut() { }
  virtual ~TrackCut() { }

  virtual bool operator()(const StSpinJet::Track& track) = 0;

};

}

#endif // TRACKCUT_H
