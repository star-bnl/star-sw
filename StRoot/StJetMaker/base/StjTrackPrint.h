// -*- mode: c++;-*-
// $Id: StjTrackPrint.h,v 1.2 2008/08/02 19:22:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTRACKPRINT_H
#define STJETTRACKPRINT_H

#include "StjTrackList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StjTrackPrint {

public:

  StjTrackPrint() { }
  virtual ~StjTrackPrint() { }

  void operator()(const StjTrackList& trackList);

private:

  void print(const StjTrack& track);

};

}

#endif // STJETTRACKPRINT_H
