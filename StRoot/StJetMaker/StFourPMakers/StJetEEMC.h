// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.1 2008/07/09 05:36:00 tai Exp $
#ifndef STJETEEMC_H
#define STJETEEMC_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetEEMC {

public:
  StJetEEMC() { }
  virtual ~StJetEEMC() { }

  virtual TowerEnergyDepositList getEnergyList() = 0;

};


}

#endif // STJETEEMC_H
