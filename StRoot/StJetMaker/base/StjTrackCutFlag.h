// -*- mode: c++;-*-
// $Id: StjTrackCutFlag.h,v 1.4 2008/08/03 00:26:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTFLAG_H
#define STJTRACKCUTFLAG_H

#include "StjTrackCut.h"

class StjTrackCutFlag : public StjTrackCut {

public:
  StjTrackCutFlag(short min = 0,
	       short max = std::numeric_limits<short>::max())
    : _min(min), _max(max) { }
  virtual ~StjTrackCutFlag() { }

  bool operator()(const StjTrack& track)
  {
    if(track.flag <= _min) return true;

    if(track.flag > _max) return true;

    return false;
  }

private:

  short _min;
  short _max;

};

#endif // STJTRACKCUTFLAG_H
