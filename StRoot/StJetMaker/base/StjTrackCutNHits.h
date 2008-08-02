// -*- mode: c++;-*-
// $Id: StjTrackCutNHits.h,v 1.2 2008/08/02 19:22:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTNHITS_H
#define TRACKCUTNHITS_H

#include "StjTrackCut.h"

namespace StJetTrackCut {

class StjTrackCutNHits : public StjTrackCut {

public:
  StjTrackCutNHits(unsigned short min = 12,
		unsigned short max = std::numeric_limits<unsigned short>::max())
    : _min(min), _max(max) { }
  virtual ~StjTrackCutNHits() { }

  bool operator()(const StSpinJet::StjTrack& track)
  {
    if(track.nHits <= _min) return true;

    if(track.nHits > _max) return true;

    return false;
  }

private:

  unsigned short _min;
  unsigned short _max;

};

}

#endif // TRACKCUTNHITS_H
