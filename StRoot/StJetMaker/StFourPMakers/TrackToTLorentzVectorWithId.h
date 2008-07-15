// -*- mode: c++;-*-
// $Id: TrackToTLorentzVectorWithId.h,v 1.1 2008/07/15 04:44:14 tai Exp $
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
  TLorentzVectorWithId operator()(const Track& track);

private:
  TrackToTLorentzVector& _track2tlorentzvector;
};

}

#endif // TRACKTOTLORENTZVECTORWITHID_H
