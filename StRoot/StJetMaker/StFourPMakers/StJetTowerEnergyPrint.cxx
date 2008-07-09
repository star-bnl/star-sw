// $Id: StJetTowerEnergyPrint.cxx,v 1.1 2008/07/09 05:13:16 tai Exp $
#include "StJetTowerEnergyPrint.h"

#include <TVector3.h>

#include <iostream>

using namespace std;

namespace StSpinJet {

void StJetTowerEnergyPrint::operator()(const TowerEnergyDepositList &energyList)
{
  if(_i == 0) _ofs.open(_fileName.c_str());

  for(TowerEnergyDepositList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    print(*it);

  }
  ++_i;
}

void StJetTowerEnergyPrint::print(const TowerEnergyDeposit& energyDeposit)
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
