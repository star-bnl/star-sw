// -*- mode: c++;-*-
// $Id: CollectEnergyDepositsFromEEMC.h,v 1.1 2008/06/10 08:07:08 tai Exp $
#ifndef COLLECTENERGYDEPOSITSFROMEEMC_H
#define COLLECTENERGYDEPOSITSFROMEEMC_H

#include "TowerEnergyDeposit.h"

class StMuDstMaker;

namespace StSpinJet {

class CollectEnergyDepositsFromEEMC {

public:
  CollectEnergyDepositsFromEEMC(StMuDstMaker* uDstMaker);
  virtual ~CollectEnergyDepositsFromEEMC() { }

  TowerEnergyDepositList Do();

private:

  StMuDstMaker* mMuDstMaker;


};

}


#endif // COLLECTENERGYDEPOSITSFROMEEMC_H
