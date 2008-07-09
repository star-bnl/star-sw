// $Id: CollectEnergyDepositsFromEEMC.cxx,v 1.8 2008/07/09 05:35:57 tai Exp $
#include "CollectEnergyDepositsFromEEMC.h"
#include "StJetEEMC.h"

namespace StSpinJet {

CollectEnergyDepositsFromEEMC::CollectEnergyDepositsFromEEMC(StJetEEMC* eemc)
  : _eemc(eemc)
{

}

TowerEnergyDepositList CollectEnergyDepositsFromEEMC::Do()
{
  TowerEnergyDepositList energyList = _eemc->getEnergyList();

  return energyList;
}


}
