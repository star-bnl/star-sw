// -*- mode: c++;-*-
// $Id: StjTrackToFourVec.h,v 1.2 2008/08/02 19:22:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOFOURVEC_H
#define TRACKTOFOURVEC_H

#include "StjFourVecList.h"

#include "StjTrackToTLorentzVector.h"

namespace StSpinJet {

class StjTrack;

class StjTrackToFourVec {
public:
  StjTrackToFourVec(double mass = 0.1395700 /* pion mass as default */)
    : _track2tlorentzvector(*(new StjTrackToTLorentzVector(mass))) { }
  virtual ~StjTrackToFourVec() { delete &_track2tlorentzvector; }
  StjFourVec operator()(const StjTrack& track);

private:
  StjTrackToTLorentzVector& _track2tlorentzvector;
};

}

#endif // TRACKTOFOURVEC_H
