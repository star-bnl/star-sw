// -*- mode: c++;-*-
// $Id: StJetTowerEnergyPrint.h,v 1.1 2008/07/09 05:13:16 tai Exp $
#ifndef STJETTOWERENERGYPRINT_H
#define STJETTOWERENERGYPRINT_H

#include "TowerEnergyDeposit.h"

#include <fstream>
#include <string>

namespace StSpinJet {

class StJetTowerEnergyPrint {

public:

  StJetTowerEnergyPrint(const char* fileName = "./energy.txt")
    : _i(0), _fileName(fileName) { }
  virtual ~StJetTowerEnergyPrint() { }

  void operator()(const TowerEnergyDepositList& energyList);

private:

  void print(const TowerEnergyDeposit& energyDeposit);

  long _i;

  std::string _fileName;
  std::ofstream _ofs;

};

}

#endif // STJETTOWERENERGYPRINT_H
