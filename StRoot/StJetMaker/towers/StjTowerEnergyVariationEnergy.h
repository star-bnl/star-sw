// -*- mode: c++;-*-
// $Id: StjTowerEnergyVariationEnergy.h,v 1.1 2008/11/27 07:35:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATIONENERGY_H
#define STJTOWERENERGYVARIATIONENERGY_H

#include "StjTowerEnergyVariation.h"

class StjTowerEnergyVariationEnergy : public StjTowerEnergyVariation {

public:
  StjTowerEnergyVariationEnergy(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StjTowerEnergyVariationEnergy() { }

  StjTowerEnergy operator()(const StjTowerEnergy& deposit)
  {
    StjTowerEnergy ret(deposit);

    ret.energy *= (1.0 + _ratio);

    return ret;
  }

private:

  double _ratio;

  ClassDef(StjTowerEnergyVariationEnergy, 1)

};

#endif // STJTOWERENERGYVARIATIONENERGY_H
