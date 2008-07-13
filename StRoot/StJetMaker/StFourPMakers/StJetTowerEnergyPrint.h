// -*- mode: c++;-*-
// $Id: StJetTowerEnergyPrint.h,v 1.3 2008/07/13 10:02:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTOWERENERGYPRINT_H
#define STJETTOWERENERGYPRINT_H

#include "TowerEnergyList.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetTowerEnergyPrint {

public:

  StJetTowerEnergyPrint(const char* fileName = "./energy.txt")
    : _i(0), _fileName(fileName) { }
  virtual ~StJetTowerEnergyPrint() { }

  void operator()(const TowerEnergyList& energyList);

private:

  void print(const TowerEnergy& energyDeposit);

  long _i;

  std::string _fileName;
  std::ofstream _ofs;

};

}

#endif // STJETTOWERENERGYPRINT_H
