// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.2 2008/07/10 01:56:08 tai Exp $
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


}

#endif // STJETBEMC_H
