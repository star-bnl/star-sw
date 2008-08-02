// -*- mode: c++;-*-
// $Id: StjTowerEnergyVariation.h,v 1.1 2008/08/02 04:16:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYVARIATION_H
#define STJETTOWERENERGYVARIATION_H

#include "StjTowerEnergyList.h"

namespace StSpinJet {

class StJetTowerEnergyVariation {

public:
  StJetTowerEnergyVariation(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StJetTowerEnergyVariation() { }
  
  TowerEnergyList operator()(const TowerEnergyList& energyList);

private:

  TowerEnergy vary(const TowerEnergy& deposit);

  double _ratio;
};

}

#endif // STJETTOWERENERGYVARIATION_H
