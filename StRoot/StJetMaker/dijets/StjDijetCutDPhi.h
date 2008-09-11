// -*- mode: c++;-*-
// $Id: StjDijetCutDPhi.h,v 1.1 2008/09/11 23:34:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTDPHI_H
#define STJDIJETCUTDPHI_H

#include "StjDijetCut.h"

class StjDijetCutDPhi : public StjDijetCut {

public:
  StjDijetCutDPhi(double min = 2.0, double max = 10.0)
    : _min(min), _max(max) { }
  virtual ~StjDijetCutDPhi() { }

  bool operator()(const StjDijet& dijet)
  {
    if(dijet.dphi < _min) return true;

    if(dijet.dphi > _max) return true;

    return false;
  }

private:

  double _min;

  double _max;

  ClassDef(StjDijetCutDPhi, 1)

};

#endif // STJDIJETCUTDPHI_H
