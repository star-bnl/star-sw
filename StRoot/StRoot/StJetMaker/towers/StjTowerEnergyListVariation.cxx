// $Id: StjTowerEnergyListVariation.cxx,v 1.1 2008/11/27 07:35:31 tai Exp $
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

  for(VarList::iterator var = _varList.begin(); var != _varList.end(); ++var){
    ret = (**var)(ret);
  }

  return ret;
}
