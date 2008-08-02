// -*- mode: c++;-*-
// $Id: StjTrackPrint.h,v 1.1 2008/08/02 04:16:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTRACKPRINT_H
#define STJETTRACKPRINT_H

#include "StjTrackList.h"

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
