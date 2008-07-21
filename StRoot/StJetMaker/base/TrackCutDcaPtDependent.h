// -*- mode: c++;-*-
// $Id: TrackCutDcaPtDependent.h,v 1.1 2008/07/21 17:24:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKCUTDCAPTDEPENDENT_H
#define TRACKCUTDCAPTDEPENDENT_H

#include "TrackCut.h"

namespace StJetTrackCut {

class TrackCutDcaPtDependent : public TrackCut {
  // to reduce pile up tracks

  //
  //
  //          dca
  //           |
  //           |
  //           |
  //           |                     pass
  //           |
  //           |
  //           |
  //     dca1  |----------- \
  //           |             \
  //           |              \
  //           |               \
  //     dca2  |                \----------------------------
  //           |
  //           |                   not pass
  //           |
  //          -+--------------------------------------------- pt
  //                      pt1 pt2
  //

public:
  TrackCutDcaPtDependent(double pt1 = 0.5, double dcaMax1 = 2.0, double pt2 = 1.0, double dcaMax2 = 1.0)
    : _pt1(pt1), _dcaMax1(dcaMax1), _pt2(pt2), _dcaMax2(dcaMax2) { }
  virtual ~TrackCutDcaPtDependent() { }

  bool operator()(const StSpinJet::Track& track)
  {
    if(track.pt < _pt1) {
      if(track.dcaD > _dcaMax1) return true;
    } else if(track.pt < _pt2) {
      if(track.dcaD*(_pt2 - _pt1) > (_pt2*_dcaMax1 - _pt1*_dcaMax2) + (_dcaMax2 - _dcaMax1)*track.pt) return true;
    } else {
       if(track.dcaD > _dcaMax2) return true;
    }

    return false;
  }

private:

  double  _pt1;
  double  _dcaMax1;
  double  _pt2;
  double  _dcaMax2;

};

}

#endif // TRACKCUTDCAPTDEPENDENT_H
