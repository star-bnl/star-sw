// -*- mode: c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M
// 22 May 2012
//

#ifndef STJ_TRACK_CUT_TDCA_PT_DEPENDENT_H
#define STJ_TRACK_CUT_TDCA_PT_DEPENDENT_H

#include <cmath>
#include "StjTrackCut.h"

class StjTrackCutTdcaPtDependent : public StjTrackCut {
  /* to reduce pile up tracks
   *
   *
   *
   *          dca
   *           |
   *           |
   *           |
   *           |                     pass
   *           |
   *           |
   *           |
   *     dca1  |-----------\
   *           |	    \
   *           |	     \
   *           |	      \
   *     dca2  |               \----------------------------
   *           |
   *           |                   not pass
   *           |
   *          -+--------------------------------------------- pt
   *                      pt1 pt2
   */

public:
  StjTrackCutTdcaPtDependent(double pt1 = 0.5, double dcaMax1 = 2.0, double pt2 = 1.5, double dcaMax2 = 1.0)
    : _pt1(pt1), _dcaMax1(dcaMax1), _pt2(pt2), _dcaMax2(dcaMax2) { }

  bool operator()(const StjTrack& track) const
  {
    if (track.pt < _pt1) return fabs(track.Tdca) > _dcaMax1;
    if (track.pt < _pt2) return fabs(track.Tdca) > fabs(_dcaMax1 + (_dcaMax2 - _dcaMax1) / (_pt2 - _pt1) * (track.pt - _pt1));
    return fabs(track.Tdca) > _dcaMax2;
  }

private:
  double  _pt1;
  double  _dcaMax1;
  double  _pt2;
  double  _dcaMax2;

  ClassDef(StjTrackCutTdcaPtDependent,0)
};

#endif // STJ_TRACK_CUT_TDCA_PT_DEPENDENT_H
