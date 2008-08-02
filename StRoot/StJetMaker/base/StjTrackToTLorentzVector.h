// -*- mode: c++;-*-
// $Id: StjTrackToTLorentzVector.h,v 1.3 2008/08/02 22:43:22 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOTLORENTZVECTOR_H
#define STJTRACKTOTLORENTZVECTOR_H

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

#endif // STJTRACKTOTLORENTZVECTOR_H
