// -*- mode: c++;-*-
// $Id: StjTrackPrint.h,v 1.4 2008/08/03 00:26:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKPRINT_H
#define STJTRACKPRINT_H

#include "StjTrackList.h"

#include <fstream>
#include <string>

class StjTrackPrint {

public:

  StjTrackPrint() { }
  virtual ~StjTrackPrint() { }

  void operator()(const StjTrackList& trackList);

private:

  void print(const StjTrack& track);

};

#endif // STJTRACKPRINT_H
