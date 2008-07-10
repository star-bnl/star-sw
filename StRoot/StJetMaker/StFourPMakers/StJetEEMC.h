// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.4 2008/07/10 19:48:18 tai Exp $
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


class StJetEEMCNull : public StJetEEMC {

public:
  StJetEEMCNull() { }
  virtual ~StJetEEMCNull() { }

  TowerEnergyDepositList getEnergyList() { return TowerEnergyDepositList(); }
};


}

#endif // STJETEEMC_H
