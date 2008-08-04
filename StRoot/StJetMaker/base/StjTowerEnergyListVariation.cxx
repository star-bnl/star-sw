// $Id: StjTowerEnergyListVariation.cxx,v 1.1 2008/08/04 20:47:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyListVariation.h"

ClassImp(StjTowerEnergyListVariation)

using namespace std;

StjTowerEnergyList StjTowerEnergyListVariation::operator()(const StjTowerEnergyList &energyList)
{
  StjTowerEnergyList ret;

  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    ret.push_back(vary(*it));
  }

  return ret;
}


StjTowerEnergy StjTowerEnergyListVariation::vary(const StjTowerEnergy& energyDeposit)
{
  StjTowerEnergy ret(energyDeposit);
  ret.energy *= (1.0 + _ratio);
  return ret;
}
