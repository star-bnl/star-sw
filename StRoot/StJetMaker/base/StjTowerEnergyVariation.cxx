// $Id: StjTowerEnergyVariation.cxx,v 1.3 2008/08/03 00:26:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyVariation.h"

using namespace std;

StjTowerEnergyList StjTowerEnergyVariation::operator()(const StjTowerEnergyList &energyList)
{
  StjTowerEnergyList ret;

  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    ret.push_back(vary(*it));
  }

  return ret;
}


StjTowerEnergy StjTowerEnergyVariation::vary(const StjTowerEnergy& energyDeposit)
{
  StjTowerEnergy ret(energyDeposit);
  ret.energy *= (1.0 + _ratio);
  return ret;
}
