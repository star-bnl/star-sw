// -*- mode: c++;-*-
// $Id: StjTrackToFourVec.h,v 1.1 2008/11/27 07:09:37 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOFOURVEC_H
#define STJTRACKTOFOURVEC_H

#include <TObject.h>

#include "StjFourVecList.h"

#include "StjTrackToTLorentzVector.h"

class StjTrack;

class StjTrackToFourVec : public TObject {
public:
  StjTrackToFourVec(double mass = 0.1395700 /* pion mass as default */)
    : _track2tlorentzvector(*(new StjTrackToTLorentzVector(mass))) { }
  virtual ~StjTrackToFourVec() { delete &_track2tlorentzvector; }
  StjFourVec operator()(const StjTrack& track);

private:
  StjTrackToTLorentzVector& _track2tlorentzvector;
  ClassDef(StjTrackToFourVec, 1)

};

#endif // STJTRACKTOFOURVEC_H
