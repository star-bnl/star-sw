// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.1 2008/07/09 00:04:16 tai Exp $
#ifndef STJETBEMC_H
#define STJETBEMC_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetBEMC {

public:
  StJetBEMC() { }
  virtual ~StJetBEMC() { }

  virtual TowerEnergyDepositList getEnergyList() = 0;

};


}

#endif // STJETBEMC_H
