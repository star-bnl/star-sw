// -*- mode: c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 12 April 2010
//

#ifndef STJ_TRACK_CUT_CHI2_H
#define STJ_TRACK_CUT_CHI2_H

#include "StjTrackCut.h"

class StjTrackCutChi2 : public StjTrackCut {
public:
  StjTrackCutChi2(double chi2min = 0, double chi2max = 4)
    : _chi2min(chi2min)
    , _chi2max(chi2max)
  {
  }

  bool operator()(const StjTrack& track) const
  {
    return track.chi2 < _chi2min || track.chi2 > _chi2max;
  }

private:
  double _chi2min;
  double _chi2max;

  ClassDef(StjTrackCutChi2,1);
};

#endif	// STJ_TRACK_CUT_CHI2_H
