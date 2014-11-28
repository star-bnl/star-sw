// -*- mode: c++;-*-
// $Id: StjTrackCutDcaPtDependent.h,v 1.4 2010/04/13 13:30:51 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTDCAPTDEPENDENT_H
#define STJTRACKCUTDCAPTDEPENDENT_H

#include <cmath>
#include "StjTrackCut.h"

class StjTrackCutDcaPtDependent : public StjTrackCut {
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
  StjTrackCutDcaPtDependent(double pt1 = 0.5, double dcaMax1 = 2.0, double pt2 = 1.0, double dcaMax2 = 1.0)
    : _pt1(pt1), _dcaMax1(dcaMax1), _pt2(pt2), _dcaMax2(dcaMax2) { }
  virtual ~StjTrackCutDcaPtDependent() { }

  bool operator()(const StjTrack& track) const
  {
    if (track.pt < _pt1) return fabs(track.dcaD) > _dcaMax1;
    if (track.pt < _pt2) return fabs(track.dcaD) > fabs(_dcaMax1 + (_dcaMax2 - _dcaMax1) / (_pt2 - _pt1) * (track.pt - _pt1));
    return fabs(track.dcaD) > _dcaMax2;
  }

private:

  double  _pt1;
  double  _dcaMax1;
  double  _pt2;
  double  _dcaMax2;

  ClassDef(StjTrackCutDcaPtDependent, 1)

};

#endif // STJTRACKCUTDCAPTDEPENDENT_H
