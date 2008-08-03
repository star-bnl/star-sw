// -*- mode: c++;-*-
// $Id: StjTrackPrint.h,v 1.5 2008/08/03 23:04:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKPRINT_H
#define STJTRACKPRINT_H

#include <TObject.h>

#include "StjTrackList.h"

#include <fstream>
#include <string>

class StjTrackPrint : public TObject {

public:

  StjTrackPrint() { }
  virtual ~StjTrackPrint() { }

  void operator()(const StjTrackList& trackList);

private:

  void print(const StjTrack& track);

  ClassDef(StjTrackPrint, 1)

};

#endif // STJTRACKPRINT_H
