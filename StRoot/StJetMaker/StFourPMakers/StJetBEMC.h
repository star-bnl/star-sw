// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.3 2008/07/10 19:35:30 tai Exp $
#ifndef STJETBEMC_H
#define STJETBEMC_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetBEMC {

public:
  StJetBEMC() { }
  virtual ~StJetBEMC() { }

  virtual void Init() { }

  virtual TowerEnergyDepositList getEnergyList() = 0;

  virtual bool isUsed() const { return true; }
};


class StJetBEMCNull : public StJetBEMC {

public:
  StJetBEMCNull() { }
  virtual ~StJetBEMCNull() { }

  TowerEnergyDepositList getEnergyList() { return TowerEnergyDepositList(); }

  bool isUsed() const { return false; }
};

}

#endif // STJETBEMC_H
