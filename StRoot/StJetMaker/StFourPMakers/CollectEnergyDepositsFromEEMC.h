// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.2 2008/06/10 08:31:07 tai Exp $
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

  StMuDstMaker* mMuDstMaker;

  EEmcGeomSimple* mEeGeom;
  StEEmcDbMaker* mEeDb;

};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
