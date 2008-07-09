// $Id: CollectEnergyDepositsFromBEMC.cxx,v 1.15 2008/07/09 23:53:30 tai Exp $
#include "CollectEnergyDepositsFromBEMC.h"

#include "StJetBEMC.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

CollectEnergyDepositsFromBEMC::CollectEnergyDepositsFromBEMC(StJetBEMC* bemc, StJetBEMCEnergyCut* cut)
  : _bemc(bemc)
  , _cut(cut)
{

}


TowerEnergyDepositList CollectEnergyDepositsFromBEMC::Do()
{
  TowerEnergyDepositList energyList = _bemc->getEnergyList();

  energyList = _cut->Apply(energyList);

  return energyList;
}

}
