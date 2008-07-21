// -*- mode: c++;-*-
// $Id: StJetTrackPrint.h,v 1.1 2008/07/21 17:24:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTRACKPRINT_H
#define STJETTRACKPRINT_H

#include "TrackList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetTrackPrint {

public:

  StJetTrackPrint() { }
  virtual ~StJetTrackPrint() { }

  void operator()(const TrackList& trackList);

private:

  void print(const Track& track);

};

}

#endif // STJETTRACKPRINT_H
