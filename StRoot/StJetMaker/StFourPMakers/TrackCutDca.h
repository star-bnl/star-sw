// -*- mode: c++;-*-
// $Id: TrackCutDca.h,v 1.1 2008/07/13 09:38:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTDCA_H
#define TRACKCUTDCA_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutDca : public TrackCut {
  // to reduce pile up tracks

public:
  TrackCutDca(double max = 3.0, double min = 0.0) : _max(max), _min(min) { }
  virtual ~TrackCutDca() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.Tdca > _max) return true;

    if(track.Tdca < _min) return true;

    return false;
  }

private:

  double  _max;
  double  _min;

};

}

#endif // TRACKCUTDCA_H
