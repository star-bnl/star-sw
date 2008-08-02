// -*- mode: c++;-*-
// $Id: StjTrackCutFlag.h,v 1.2 2008/08/02 19:22:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTFLAG_H
#define TRACKCUTFLAG_H

#include "StjTrackCut.h"

namespace StJetTrackCut {

class StjTrackCutFlag : public StjTrackCut {

public:
  StjTrackCutFlag(short min = 0,
	       short max = std::numeric_limits<short>::max())
    : _min(min), _max(max) { }
  virtual ~StjTrackCutFlag() { }

  bool operator()(const StSpinJet::StjTrack& track)
  {
    if(track.flag <= _min) return true;

    if(track.flag > _max) return true;

    return false;
  }

private:

  short _min;
  short _max;

};

}

#endif // TRACKCUTFLAG_H
