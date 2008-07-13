// $Id: StJetTowerEnergyPrint.cxx,v 1.3 2008/07/13 05:36:46 tai Exp $
#include "StJetTowerEnergyPrint.h"

#include <TVector3.h>

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetTowerEnergyPrint::operator()(const TowerEnergyList &energyList)
{
  if(_i == 0) _ofs.open(_fileName.c_str());

  for(TowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    print(*it);

  }
  ++_i;
}

void StJetTowerEnergyPrint::print(const TowerEnergy& energyDeposit)
{
  _ofs 
    << _i << " "
    << energyDeposit.towerId << " "
    << energyDeposit.towerR << " "
    << energyDeposit.towerEta << " "
    << energyDeposit.towerPhi << " "
    << energyDeposit.vertexX << " "
    << energyDeposit.vertexY << " "
    << energyDeposit.vertexZ << " "
    << energyDeposit.energy << " "
    << energyDeposit.adc << " "
    << energyDeposit.pedestal << " "
    << energyDeposit.rms << " "
    << energyDeposit.status << " "
    << endl;

}



}
