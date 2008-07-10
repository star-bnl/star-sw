// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.2 2008/07/10 01:56:09 tai Exp $
#ifndef STJETEEMC_H
#define STJETEEMC_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetEEMC {

public:
  StJetEEMC() { }
  virtual ~StJetEEMC() { }

  virtual void Init() { }

  virtual TowerEnergyDepositList getEnergyList() = 0;

};


}

#endif // STJETEEMC_H
