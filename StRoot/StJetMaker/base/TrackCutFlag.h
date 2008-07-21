// -*- mode: c++;-*-
// $Id: TrackCutFlag.h,v 1.1 2008/07/21 17:25:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTFLAG_H
#define TRACKCUTFLAG_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutFlag : public TrackCut {

public:
  TrackCutFlag(short min = 0,
	       short max = std::numeric_limits<short>::max())
    : _min(min), _max(max) { }
  virtual ~TrackCutFlag() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.nHits <= _min) return true;

    if(track.nHits > _max) return true;

    return false;
  }

private:

  short _min;
  short _max;

};

}

#endif // TRACKCUTFLAG_H
