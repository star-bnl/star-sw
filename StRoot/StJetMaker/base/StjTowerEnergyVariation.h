// -*- mode: c++;-*-
// $Id: StjTowerEnergyVariation.h,v 1.4 2008/08/03 00:26:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATION_H
#define STJTOWERENERGYVARIATION_H

#include "StjTowerEnergyList.h"

class StjTowerEnergyVariation {

public:
  StjTowerEnergyVariation(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StjTowerEnergyVariation() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

private:

  StjTowerEnergy vary(const StjTowerEnergy& deposit);

  double _ratio;
};

#endif // STJTOWERENERGYVARIATION_H
