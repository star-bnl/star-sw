// $Id: StjTowerEnergyPrint.cxx,v 1.4 2008/08/04 00:55:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyPrint.h"

#include <iostream>

ClassImp(StjTowerEnergyPrint)

using namespace std;

void StjTowerEnergyPrint::operator()(const StjTowerEnergyList &energyList)
{
  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    print(*it);
  }
}

void StjTowerEnergyPrint::print(const StjTowerEnergy& energyDeposit)
{
  cout 
    << energyDeposit.runNumber      << " "
    << energyDeposit.eventId        << " "
    << energyDeposit.detectorId     << " "
    << energyDeposit.towerId        << " "
    << energyDeposit.towerR         << " "
    << energyDeposit.towerEta       << " "
    << energyDeposit.towerPhi       << " "
    << energyDeposit.vertexX        << " "
    << energyDeposit.vertexY        << " "
    << energyDeposit.vertexZ        << " "
    << energyDeposit.energy         << " "
    << energyDeposit.adc            << " "
    << energyDeposit.pedestal       << " "
    << energyDeposit.rms            << " "
    << energyDeposit.status         << " "
    << endl;

}
