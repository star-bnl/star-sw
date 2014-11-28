// -*- mode: c++;-*-
// $Id: StjTrackToTLorentzVector.h,v 1.1 2008/11/27 07:09:38 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOTLORENTZVECTOR_H
#define STJTRACKTOTLORENTZVECTOR_H

#include <TObject.h>

#include <TLorentzVector.h>

class StjTrack;

class StjTrackToTLorentzVector : public TObject {
public:
  StjTrackToTLorentzVector(double mass = 0.1395700 /* pion mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const StjTrack& track);

private:
  double _mass;

  ClassDef(StjTrackToTLorentzVector, 1)

};

#endif // STJTRACKTOTLORENTZVECTOR_H
