// -*- mode:c++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 April 2010
//

//
// See http://www.star.bnl.gov/HyperNews-star/protected/get/jetfinding/992/1.html
//

#ifndef STJ_TRACK_CUT_LAST_POINT_H
#define STJ_TRACK_CUT_LAST_POINT_H

#include "StjTrackCut.h"

class StjTrackCutLastPoint : public StjTrackCut {
public:
  StjTrackCutLastPoint(double radius = 125) : _radius(radius) {}

  bool operator()(const StjTrack& track) const {  return track.lastPoint.Perp() <= _radius; }

private:
  double _radius;

  ClassDef(StjTrackCutLastPoint,0)
};

#endif	// STJ_TRACK_CUT_LAST_POINT_H
