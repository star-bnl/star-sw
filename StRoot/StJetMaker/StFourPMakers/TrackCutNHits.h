// -*- mode: c++;-*-
// $Id: TrackCutNHits.h,v 1.1 2008/07/16 21:50:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTNHITS_H
#define TRACKCUTNHITS_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutNHits : public TrackCut {

public:
  TrackCutNHits(unsigned short min = 12,
		unsigned short max = std::numeric_limits<unsigned short>::max())
    : _min(min), _max(max) { }
  virtual ~TrackCutNHits() { }

  bool operator()(const StSpinJet::Track& track)
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
