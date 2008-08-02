// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutEnergy.h,v 1.1 2008/08/02 04:15:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTENERGY_H
#define TOWERENERGYCUTENERGY_H

#include "StjTowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class TowerEnergyCutEnergy : public TowerEnergyCut {

public:
  TowerEnergyCutEnergy(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~TowerEnergyCutEnergy() { }

  bool operator()(const StSpinJet::TowerEnergy& deposit)
  {
    if(deposit.energy <= _min) return true;

    if(deposit.energy > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

}

#endif // TOWERENERGYCUTENERGY_H
