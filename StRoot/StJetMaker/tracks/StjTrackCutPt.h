// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 28 May 2010
//

#ifndef STJ_TRACK_CUT_PT_H
#define STJ_TRACK_CUT_PT_H

#include "StjTrackCut.h"

class StjTrackCutPt : public StjTrackCut {
public:
  StjTrackCutPt(double ptmin, double ptmax) : _ptMin(ptmin), _ptMax(ptmax) {}

  bool operator()(const StjTrack& track) const
  {
    return track.pt <= _ptMin || track.pt >= _ptMax;
  }

private:
  double _ptMin;
  double _ptMax;

  ClassDef(StjTrackCutPt,0);
};

#endif	// STJ_TRACK_CUT_PT_H
