// -*- mode: c++;-*-
// $Id: StjTrackCutNHits.h,v 1.4 2008/08/03 00:26:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTNHITS_H
#define STJTRACKCUTNHITS_H

#include "StjTrackCut.h"

class StjTrackCutNHits : public StjTrackCut {

public:
  StjTrackCutNHits(unsigned short min = 12,
		unsigned short max = std::numeric_limits<unsigned short>::max())
    : _min(min), _max(max) { }
  virtual ~StjTrackCutNHits() { }

  bool operator()(const StjTrack& track)
  {
    if(track.nHits <= _min) return true;

    if(track.nHits > _max) return true;

    return false;
  }

private:

  unsigned short _min;
  unsigned short _max;

};

#endif // STJTRACKCUTNHITS_H
