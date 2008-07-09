// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.7 2008/07/09 07:06:44 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

#include "StJetTowerEnergyPrint.h"

namespace StSpinJet {

class StJetEEMC;

class CollectEnergyDepositsFromEEMC {

public:
  CollectEnergyDepositsFromEEMC(StJetEEMC* eemc);
  virtual ~CollectEnergyDepositsFromEEMC() { }

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList getEnergyList();

  StJetEEMC* _eemc;

  StJetTowerEnergyPrint _print;

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
