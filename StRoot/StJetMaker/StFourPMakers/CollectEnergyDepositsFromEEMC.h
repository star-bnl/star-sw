// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.6 2008/07/09 05:35:57 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

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

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
