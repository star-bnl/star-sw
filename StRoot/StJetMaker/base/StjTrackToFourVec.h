// -*- mode: c++;-*-
// $Id: StjTrackToFourVec.h,v 1.4 2008/08/03 00:26:38 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOFOURVEC_H
#define STJTRACKTOFOURVEC_H

#include "StjFourVecList.h"

#include "StjTrackToTLorentzVector.h"

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

#endif // STJTRACKTOFOURVEC_H
