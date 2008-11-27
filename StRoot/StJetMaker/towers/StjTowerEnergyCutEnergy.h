// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutEnergy.h,v 1.1 2008/11/27 07:35:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTENERGY_H
#define STJTOWERENERGYCUTENERGY_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutEnergy : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutEnergy(double min = 0, double max = 50000.0)
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

  ClassDef(StjTowerEnergyCutEnergy, 1)

};

#endif // STJTOWERENERGYCUTENERGY_H
