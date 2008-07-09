// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromBEMC.h,v 1.8 2008/07/09 00:04:16 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMBEMC_H
#define COLLECTENERGYDEPOSITSFROMBEMC_H

#include "TowerEnergyDeposit.h"

#include <TVector3.h>

namespace StSpinJet {

class StJetBEMC;

class CollectEnergyDepositsFromBEMC {

public:
  CollectEnergyDepositsFromBEMC(StJetBEMC* bemc);
  virtual ~CollectEnergyDepositsFromBEMC() { }

  void setUse2003Cuts(bool v) { mUse2003Cuts = v; }
  void setUse2005Cuts(bool v) { mUse2005Cuts = v; }

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList selectBemcTowerHits(const TowerEnergyDepositList &energyList);
  bool shouldKeepThisBemcHit(const TowerEnergyDeposit& energyDeposit);

  bool accept2003Tower(int id);

  StJetBEMC* _bemc;


  bool mUse2003Cuts;
  bool mUse2005Cuts;

};

}


#endif // COLLECTENERGYDEPOSITSFROMBEMC_H
