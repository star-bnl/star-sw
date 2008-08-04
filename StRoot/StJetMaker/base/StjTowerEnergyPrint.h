// -*- mode: c++;-*-
// $Id: StjTowerEnergyPrint.h,v 1.5 2008/08/04 00:55:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYPRINT_H
#define STJTOWERENERGYPRINT_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

#include <fstream>
#include <string>

class StjTowerEnergyPrint : public TObject {

public:

  StjTowerEnergyPrint() { }
  virtual ~StjTowerEnergyPrint() { }

  void operator()(const StjTowerEnergyList& energyList);

private:

  void print(const StjTowerEnergy& energyDeposit);

  ClassDef(StjTowerEnergyPrint, 1)

};

#endif // STJTOWERENERGYPRINT_H
