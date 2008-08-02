// -*- mode: c++;-*-
// $Id: StjTrackCutFlag.h,v 1.3 2008/08/02 22:43:21 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTFLAG_H
#define STJTRACKCUTFLAG_H

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

#endif // STJTRACKCUTFLAG_H
