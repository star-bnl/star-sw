// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.3 2008/07/10 18:48:31 tai Exp $
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

  virtual bool isUsed() const { return true; }
};


class StJetEEMCNull : public StJetEEMC {

public:
  StJetEEMCNull() { }
  virtual ~StJetEEMCNull() { }

  TowerEnergyDepositList getEnergyList() { return TowerEnergyDepositList(); }

  bool isUsed() const { return false; }
};


}

#endif // STJETEEMC_H
