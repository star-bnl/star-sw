// -*- mode: c++;-*-
// $Id: TrackToTLorentzVectorWithId.h,v 1.2 2008/07/15 08:39:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOTLORENTZVECTORWITHID_H
#define TRACKTOTLORENTZVECTORWITHID_H

#include "TLorentzVectorWithId.h"

#include "TrackToTLorentzVector.h"

namespace StSpinJet {

class Track;

class TrackToTLorentzVectorWithId {
public:
  TrackToTLorentzVectorWithId(double mass = 0.1395700 /* pion mass as default */)
    : _track2tlorentzvector(*(new TrackToTLorentzVector(mass))) { }
  virtual ~TrackToTLorentzVectorWithId() { delete &_track2tlorentzvector; }
  TLorentzVectorWithId operator()(const Track& track);

private:
  TrackToTLorentzVector& _track2tlorentzvector;
};

}

#endif // TRACKTOTLORENTZVECTORWITHID_H
