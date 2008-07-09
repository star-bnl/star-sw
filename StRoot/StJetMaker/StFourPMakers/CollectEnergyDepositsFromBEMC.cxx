// $Id: CollectEnergyDepositsFromBEMC.cxx,v 1.14 2008/07/09 10:24:30 tai Exp $
#include "CollectEnergyDepositsFromBEMC.h"

#include "StJetBEMC.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

CollectEnergyDepositsFromBEMC::CollectEnergyDepositsFromBEMC(StJetBEMC* bemc)
  : _bemc(bemc)
  , _cut(new StJetBEMCEnergyCut)
{

}


TowerEnergyDepositList CollectEnergyDepositsFromBEMC::Do()
{
  TowerEnergyDepositList energyList = _bemc->getEnergyList();

  energyList = _cut->Apply(energyList);

  return energyList;
}

}
