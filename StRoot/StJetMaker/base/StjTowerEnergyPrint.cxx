// $Id: StjTowerEnergyPrint.cxx,v 1.1 2008/08/02 04:16:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyPrint.h"

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetTowerEnergyPrint::operator()(const TowerEnergyList &energyList)
{
  for(TowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    print(*it);
  }
}

void StJetTowerEnergyPrint::print(const TowerEnergy& energyDeposit)
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
