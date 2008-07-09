// -*- mode: c++;-*-
// $Id: StJetBEMCEnergyPrint.h,v 1.1 2008/07/09 01:53:27 tai Exp $
#ifndef STJETBEMCENERGYPRINT_H
#define STJETBEMCENERGYPRINT_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetBEMCEnergyPrint {

public:

  StJetBEMCEnergyPrint() { }
  virtual ~StJetBEMCEnergyPrint() { }

  void operator()(const TowerEnergyDepositList& energyList);

private:

  void print(const TowerEnergyDeposit& energyDeposit, long i) const;

};

}

#endif // STJETBEMCENERGYPRINT_H
