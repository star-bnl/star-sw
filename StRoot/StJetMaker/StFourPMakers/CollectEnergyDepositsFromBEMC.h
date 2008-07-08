// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromBEMC.h,v 1.4 2008/07/08 23:04:54 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMBEMC_H
#define COLLECTENERGYDEPOSITSFROMBEMC_H

#include "TowerEnergyDeposit.h"

#include <TVector3.h>

class StEmcRawHit;
class StMuDstMaker;
class StBemcTables;

namespace StSpinJet {

class CollectEnergyDepositsFromBEMC {

public:
  CollectEnergyDepositsFromBEMC(StMuDstMaker* uDstMaker, StBemcTables* bemcTables);
  virtual ~CollectEnergyDepositsFromBEMC() { }

  void setUse2003Cuts(bool v) { mUse2003Cuts = v; }
  void setUse2005Cuts(bool v) { mUse2005Cuts = v; }

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList getTowerHitsFromBEMC();

  TowerEnergyDepositList selectBemcTowerHits(const TowerEnergyDepositList &energyList);
  bool shouldKeepThisBemcHit(const TowerEnergyDeposit& energyDeposit);

  TowerEnergyDeposit readTowerHit(const StEmcRawHit& hit);

  bool accept2003Tower(int id);

  TVector3 getBemcTowerLocation(int bemcTowerId);

  StMuDstMaker* mMuDstMaker;

  StBemcTables* _bemcTables;

  bool mUse2003Cuts;
  bool mUse2005Cuts;

};

}


#endif // COLLECTENERGYDEPOSITSFROMBEMC_H
