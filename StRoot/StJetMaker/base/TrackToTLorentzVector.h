// -*- mode: c++;-*-
// $Id: TrackToTLorentzVector.h,v 1.1 2008/07/21 17:25:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOTLORENTZVECTOR_H
#define TRACKTOTLORENTZVECTOR_H

#include <TLorentzVector.h>

namespace StSpinJet {

class Track;

class TrackToTLorentzVector {
public:
  TrackToTLorentzVector(double mass = 0.1395700 /* pion mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const Track& track);

private:
  double _mass;

};

}

#endif // TRACKTOTLORENTZVECTOR_H
