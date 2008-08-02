// -*- mode: c++;-*-
// $Id: StjTPCTrackPrint.h,v 1.1 2008/08/02 04:15:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKPRINT_H
#define STJETTPCTRACKPRINT_H

#include "StjTrackList.h"

#include <vector>

namespace StSpinJet {

class StJetTPCTrackPrint {

public:

  StJetTPCTrackPrint() { }
  virtual ~StJetTPCTrackPrint() { }

  void operator()(const TrackList& trackList);

private:

  void print(const Track& track) const;

};

}

#endif // STJETTPCTRACKPRINT_H
