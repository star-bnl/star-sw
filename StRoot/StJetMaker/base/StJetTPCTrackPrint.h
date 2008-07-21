// -*- mode: c++;-*-
// $Id: StJetTPCTrackPrint.h,v 1.1 2008/07/21 17:24:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPCTRACKPRINT_H
#define STJETTPCTRACKPRINT_H

#include "TrackList.h"

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
