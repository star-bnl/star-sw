// -*- mode: c++;-*-
// $Id: StjTrackCutEta.h,v 1.2 2010/04/13 13:30:51 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTETA_H
#define STJTRACKCUTETA_H

#include "StjTrackCut.h"

class StjTrackCutEta : public StjTrackCut {

public:
  StjTrackCutEta(double min = -2.0, double max = 2.0) :_min(min), _max(max) { }
  virtual ~StjTrackCutEta() { }

  bool operator()(const StjTrack& track) const
  {
    if(track.eta < _min) return true;

    if(track.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  ClassDef(StjTrackCutEta, 1)

};

#endif // STJTRACKCUTETA_H
