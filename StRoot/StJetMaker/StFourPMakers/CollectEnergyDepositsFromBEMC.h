// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromBEMC.h,v 1.9 2008/07/09 10:24:31 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMBEMC_H
#define COLLECTENERGYDEPOSITSFROMBEMC_H

#include "TowerEnergyDeposit.h"
#include "StJetBEMCEnergyCut.h"

#include <TVector3.h>

namespace StSpinJet {

class StJetBEMC;

class CollectEnergyDepositsFromBEMC {

public:
  CollectEnergyDepositsFromBEMC(StJetBEMC* bemc);
  virtual ~CollectEnergyDepositsFromBEMC() { }

  void setUse2003Cuts(bool v) { _cut->setUse2003Cuts(v); }
  void setUse2005Cuts(bool v) { _cut->setUse2005Cuts(v); }

  TowerEnergyDepositList Do();

private:

  StJetBEMC* _bemc;
  StJetBEMCEnergyCut* _cut;

};

}


#endif // COLLECTENERGYDEPOSITSFROMBEMC_H
