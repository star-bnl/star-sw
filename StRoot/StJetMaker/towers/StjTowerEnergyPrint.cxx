// $Id: StjTowerEnergyPrint.cxx,v 1.1 2008/11/27 07:35:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTowerEnergyPrint.h"

#include <iostream>
#include <iomanip>

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
    << setw(7) << energyDeposit.runNumber      << ", "
    << setw(7) << energyDeposit.eventId        << ", "
    << setw(4) << energyDeposit.detectorId     << ", "
    << setw(4) << energyDeposit.towerId        << ", "
    << setw(7) << energyDeposit.towerR         << ", "
    << setw(7) << energyDeposit.towerEta       << ", "
    << setw(7) << energyDeposit.towerPhi       << ", "
    << setw(7) << energyDeposit.vertexX        << ", "
    << setw(7) << energyDeposit.vertexY        << ", "
    << setw(7) << energyDeposit.vertexZ        << ", "
    << setw(10) << energyDeposit.energy        << ", "
    << setw(7) << energyDeposit.adc            << ", "
    << setw(7) << energyDeposit.pedestal       << ", "
    << setw(7) << energyDeposit.rms            << ", "
    << setw(3) << energyDeposit.status
    << endl;

}
