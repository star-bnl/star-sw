// $Id: StJetTowerEnergyVariation.cxx,v 1.1 2008/07/21 17:24:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTowerEnergyVariation.h"

using namespace std;

namespace StSpinJet {


StSpinJet::TowerEnergyList StJetTowerEnergyVariation::operator()(const TowerEnergyList &energyList)
{
  TowerEnergyList ret;

  for(TowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    ret.push_back(vary(*it));
  }

  return ret;
}


TowerEnergy StJetTowerEnergyVariation::vary(const TowerEnergy& energyDeposit)
{
  TowerEnergy ret(energyDeposit);
  ret.energy *= (1.0 + _ratio);
  return ret;
}


}
