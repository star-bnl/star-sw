// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromBEMC.h,v 1.2 2008/06/10 08:07:08 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMBEMC_H
#define COLLECTENERGYDEPOSITSFROMBEMC_H

#include "TowerEnergyDeposit.h"

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

  typedef int BemcTowerID;
  typedef std::map<BemcTowerID, const StEmcRawHit*> BemcTowerIdHitMap;

  BemcTowerIdHitMap getTowerHitsFromBEMC();
  BemcTowerIdHitMap selectBemcTowerHits(const BemcTowerIdHitMap &bemcTowerHits);

  bool shouldKeepThisBemcHit(const StEmcRawHit* theRawHit, int bemcTowerID);

  StSpinJet::TowerEnergyDepositList readBemcTowerEnergy(const BemcTowerIdHitMap &bemcTowerHits);

  bool accept2003Tower(int id);

  TVector3 getBemcTowerLocation(int bemcTowerId);

  StMuDstMaker* mMuDstMaker;

  StBemcTables* _bemcTables;

  bool mUse2003Cuts;
  bool mUse2005Cuts;

};

}


#endif // COLLECTENERGYDEPOSITSFROMBEMC_H
