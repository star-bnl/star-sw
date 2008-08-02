// -*- mode: c++;-*-
// $Id: StjTPCTrackPrint.h,v 1.2 2008/08/02 19:22:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKPRINT_H
#define STJETTPCTRACKPRINT_H

#include "StjTrackList.h"

#include <vector>

namespace StSpinJet {

class StjTPCTrackPrint {

public:

  StjTPCTrackPrint() { }
  virtual ~StjTPCTrackPrint() { }

  void operator()(const StjTrackList& trackList);

private:

  void print(const StjTrack& track) const;

};

}

#endif // STJETTPCTRACKPRINT_H
