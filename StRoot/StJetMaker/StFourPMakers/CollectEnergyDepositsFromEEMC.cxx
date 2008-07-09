// $Id: CollectEnergyDepositsFromEEMC.cxx,v 1.9 2008/07/09 07:06:43 tai Exp $
#include "CollectEnergyDepositsFromEEMC.h"
#include "StJetEEMC.h"

namespace StSpinJet {

CollectEnergyDepositsFromEEMC::CollectEnergyDepositsFromEEMC(StJetEEMC* eemc)
  : _eemc(eemc)
  , _print("./eemcenergy.txt")
{

}

TowerEnergyDepositList CollectEnergyDepositsFromEEMC::Do()
{
  TowerEnergyDepositList energyList = _eemc->getEnergyList();

  //  _print(energyList);

  return energyList;
}


}
