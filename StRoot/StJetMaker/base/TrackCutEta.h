// -*- mode: c++;-*-
// $Id: TrackCutEta.h,v 1.1 2008/07/21 17:25:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTETA_H
#define TRACKCUTETA_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutEta : public TrackCut {

public:
  TrackCutEta(double min = -2.0, double max = 2.0) :_min(min), _max(max) { }
  virtual ~TrackCutEta() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.eta < _min) return true;

    if(track.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

};

}

#endif // TRACKCUTETA_H
