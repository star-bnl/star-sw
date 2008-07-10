// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.4 2008/07/10 19:48:18 tai Exp $
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
};


class StJetBEMCNull : public StJetBEMC {

public:
  StJetBEMCNull() { }
  virtual ~StJetBEMCNull() { }

  TowerEnergyDepositList getEnergyList() { return TowerEnergyDepositList(); }
};

}

#endif // STJETBEMC_H
