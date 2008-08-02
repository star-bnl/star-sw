// $Id: StjTowerEnergyPrint.cxx,v 1.2 2008/08/02 19:22:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

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



}
