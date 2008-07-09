// $Id: StJetBEMCEnergyPrint.cxx,v 1.1 2008/07/09 01:53:28 tai Exp $
#include "StJetBEMCEnergyPrint.h"

#include <TVector3.h>

#include <iostream>
#include <fstream>

using namespace std;

namespace StSpinJet {

void StJetBEMCEnergyPrint::operator()(const TowerEnergyDepositList &energyList)
{
  static long i(0);
  for(TowerEnergyDepositList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {

    print(*it, i);

  }
  ++i;
}

void StJetBEMCEnergyPrint::print(const TowerEnergyDeposit& energyDeposit, long i) const
{
  static ofstream ofs("./bemcenergy.txt");

  ofs 
    << i << " "
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
