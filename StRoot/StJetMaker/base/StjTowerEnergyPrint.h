// -*- mode: c++;-*-
// $Id: StjTowerEnergyPrint.h,v 1.2 2008/08/02 19:22:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYPRINT_H
#define STJETTOWERENERGYPRINT_H

#include "StjTowerEnergyList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StjTowerEnergyPrint {

public:

  StjTowerEnergyPrint() { }
  virtual ~StjTowerEnergyPrint() { }

  void operator()(const StjTowerEnergyList& energyList);

private:

  void print(const StjTowerEnergy& energyDeposit);

};

}

#endif // STJETTOWERENERGYPRINT_H
