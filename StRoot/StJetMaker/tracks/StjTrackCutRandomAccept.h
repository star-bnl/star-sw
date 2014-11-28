// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 3 Dec 2012
//

#ifndef STJ_TRACK_CUT_RANDOM_ACCEPT_H
#define STJ_TRACK_CUT_RANDOM_ACCEPT_H

#include <cassert>
#include "TRandom3.h"
#include "StjTrackCut.h"

class StjTrackCutRandomAccept : public StjTrackCut {
public:
  StjTrackCutRandomAccept(double fraction = 1) : _fraction(fraction), _random(0)
  {
    assert(_fraction >= 0 && _fraction <= 1);
  }

  bool operator()(const StjTrack& track) const
  {
    return _random.Rndm() >= _fraction;
  }

private:
  double _fraction;
  mutable TRandom3 _random;

  ClassDef(StjTrackCutRandomAccept,0)
};

#endif // STJ_TRACK_CUT_RANDOM_ACCEPT_H
