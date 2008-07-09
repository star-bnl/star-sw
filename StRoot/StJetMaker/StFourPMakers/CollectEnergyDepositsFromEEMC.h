// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.3 2008/07/09 04:26:39 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

class StMuDstMaker;
class EEmcGeomSimple;
class StEEmcDbMaker;

namespace StSpinJet {

class CollectEnergyDepositsFromEEMC {

public:
  CollectEnergyDepositsFromEEMC(StMuDstMaker* uDstMaker);
  virtual ~CollectEnergyDepositsFromEEMC() { }

  void Init(StEEmcDbMaker* eedb);

  TowerEnergyDepositList Do();

private:

  TowerEnergyDepositList CollectEnergyDepositsFromEEMC::getEnergyList();

  StMuDstMaker* mMuDstMaker;

  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
