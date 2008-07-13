// -*- mode: c++;-*-
// $Id: TrackCutPossibleHitRatio.h,v 1.1 2008/07/13 09:38:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTPOSSIBLEHITRATIO_H
#define TRACKCUTPOSSIBLEHITRATIO_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutPossibleHitRatio : public TrackCut {
  // to avoid split tracks

public:
  TrackCutPossibleHitRatio(double minRatio = 0.51) :_minRatio(minRatio) { }
  virtual ~TrackCutPossibleHitRatio() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(static_cast<double>(track.nHits)/static_cast<double>(track.nHitsPoss) < .51) return true;

    return false;
  }

private:

  double  _minRatio;

};

}

#endif // TRACKCUTPOSSIBLEHITRATIO_H
