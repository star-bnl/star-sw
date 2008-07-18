// -*- mode: c++;-*-
// $Id: StJetTowerEnergyPrint.h,v 1.4 2008/07/18 01:39:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYPRINT_H
#define STJETTOWERENERGYPRINT_H

#include "TowerEnergyList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetTowerEnergyPrint {

public:

  StJetTowerEnergyPrint() { }
  virtual ~StJetTowerEnergyPrint() { }

  void operator()(const TowerEnergyList& energyList);

private:

  void print(const TowerEnergy& energyDeposit);

};

}

#endif // STJETTOWERENERGYPRINT_H
