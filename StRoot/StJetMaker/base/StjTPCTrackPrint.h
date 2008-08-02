// -*- mode: c++;-*-
// $Id: StjTPCTrackPrint.h,v 1.3 2008/08/02 22:43:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCTRACKPRINT_H
#define STJTPCTRACKPRINT_H

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

#endif // STJTPCTRACKPRINT_H
