// -*- mode: c++;-*-
// $Id: StjTrackCutNHits.h,v 1.2 2010/04/13 13:30:51 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTNHITS_H
#define STJTRACKCUTNHITS_H

#include "StjTrackCut.h"

class StjTrackCutNHits : public StjTrackCut {

public:
  StjTrackCutNHits(UShort_t min = 12,
		   UShort_t max = kMaxUShort)
    : _min(min), _max(max) { }
  virtual ~StjTrackCutNHits() { }

  bool operator()(const StjTrack& track) const
  {
    if(track.nHits <= _min) return true;

    if(track.nHits > _max) return true;

    return false;
  }

private:

  UShort_t _min;
  UShort_t _max;

  ClassDef(StjTrackCutNHits, 1)

};

#endif // STJTRACKCUTNHITS_H
