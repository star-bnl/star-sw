// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.4 2008/07/09 05:13:14 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

class StMuDstMaker;
class EEmcGeomSimple;
class StEEmcDbMaker;

namespace StSpinJet {

class StJetEEMC {

public:
  StJetEEMC(StMuDstMaker* uDstMaker);
  virtual ~StJetEEMC() { }

  void Init(StEEmcDbMaker* eedb);

  TowerEnergyDepositList getEnergyList();


private:

  StMuDstMaker* mMuDstMaker;
  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

};

class CollectEnergyDepositsFromEEMC {

public:
  CollectEnergyDepositsFromEEMC(StJetEEMC* eemc);
  virtual ~CollectEnergyDepositsFromEEMC() { }

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList getEnergyList();

  StJetEEMC* _eemc;

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
