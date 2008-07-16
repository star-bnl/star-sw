// -*- mode: c++;-*-
// $Id: StJetTowerEnergyVariation.h,v 1.1 2008/07/16 18:18:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYVARIATION_H
#define STJETTOWERENERGYVARIATION_H

#include "TowerEnergyList.h"

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
