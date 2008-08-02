// -*- mode: c++;-*-
// $Id: StjTrackCutPossibleHitRatio.h,v 1.2 2008/08/02 19:22:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTPOSSIBLEHITRATIO_H
#define TRACKCUTPOSSIBLEHITRATIO_H

#include "StjTrackCut.h"

namespace StJetTrackCut {

class StjTrackCutPossibleHitRatio : public StjTrackCut {
  // to avoid split tracks

public:
  StjTrackCutPossibleHitRatio(double minRatio = 0.51) :_minRatio(minRatio) { }
  virtual ~StjTrackCutPossibleHitRatio() { }

  bool operator()(const StSpinJet::StjTrack& track)
  {
    if(static_cast<double>(track.nHits)/static_cast<double>(track.nHitsPoss) < .51) return true;

    return false;
  }

private:

  double  _minRatio;

};

}

#endif // TRACKCUTPOSSIBLEHITRATIO_H
