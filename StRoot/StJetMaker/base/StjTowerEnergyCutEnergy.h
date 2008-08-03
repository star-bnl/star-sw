// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutEnergy.h,v 1.4 2008/08/03 00:26:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTENERGY_H
#define STJTOWERENERGYCUTENERGY_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutEnergy : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutEnergy(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~StjTowerEnergyCutEnergy() { }

  bool operator()(const StjTowerEnergy& deposit)
  {
    if(deposit.energy <= _min) return true;

    if(deposit.energy > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

#endif // STJTOWERENERGYCUTENERGY_H
