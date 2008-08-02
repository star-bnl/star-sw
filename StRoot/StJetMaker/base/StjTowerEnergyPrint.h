// -*- mode: c++;-*-
// $Id: StjTowerEnergyPrint.h,v 1.1 2008/08/02 04:16:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYPRINT_H
#define STJETTOWERENERGYPRINT_H

#include "StjTowerEnergyList.h"

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
