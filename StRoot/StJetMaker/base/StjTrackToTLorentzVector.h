// -*- mode: c++;-*-
// $Id: StjTrackToTLorentzVector.h,v 1.2 2008/08/02 19:22:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOTLORENTZVECTOR_H
#define TRACKTOTLORENTZVECTOR_H

#include <TLorentzVector.h>

namespace StSpinJet {

class StjTrack;

class StjTrackToTLorentzVector {
public:
  StjTrackToTLorentzVector(double mass = 0.1395700 /* pion mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const StjTrack& track);

private:
  double _mass;

};

}

#endif // TRACKTOTLORENTZVECTOR_H
