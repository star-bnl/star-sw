// -*- mode: c++;-*-
// $Id: StjTowerEnergyPrint.h,v 1.3 2008/08/02 22:43:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYPRINT_H
#define STJTOWERENERGYPRINT_H

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

#endif // STJTOWERENERGYPRINT_H
