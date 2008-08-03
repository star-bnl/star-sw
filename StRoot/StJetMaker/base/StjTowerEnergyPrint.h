// -*- mode: c++;-*-
// $Id: StjTowerEnergyPrint.h,v 1.4 2008/08/03 00:26:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYPRINT_H
#define STJTOWERENERGYPRINT_H

#include "StjTowerEnergyList.h"

#include <fstream>
#include <string>

class StjTowerEnergyPrint {

public:

  StjTowerEnergyPrint() { }
  virtual ~StjTowerEnergyPrint() { }

  void operator()(const StjTowerEnergyList& energyList);

private:

  void print(const StjTowerEnergy& energyDeposit);

};

#endif // STJTOWERENERGYPRINT_H
