// $Id: StJetTowerEnergyPrint.cxx,v 1.2 2008/07/10 20:15:22 tai Exp $
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
    << energyDeposit.towerX << " "
    << energyDeposit.towerY << " "
    << energyDeposit.towerZ << " "
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
