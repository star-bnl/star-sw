// -*- mode: c++;-*-
// $Id: StjTrackPrint.h,v 1.3 2008/08/02 22:43:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKPRINT_H
#define STJTRACKPRINT_H

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

#endif // STJTRACKPRINT_H
