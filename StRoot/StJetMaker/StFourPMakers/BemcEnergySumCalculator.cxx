// $Id: BemcEnergySumCalculator.cxx,v 1.4 2008/07/10 06:47:47 tai Exp $
#include "BemcEnergySumCalculator.h"

#include "StJetBEMC.h"
#include "StJetBEMCEnergyCut.h"

namespace StSpinJet {

BemcEnergySumCalculatorImp::BemcEnergySumCalculatorImp(StJetBEMC* bemc, StJetBEMCEnergyCut* cut)
  : _bemc(bemc)
  , _cut(cut)
  , _DylanPoints(0)
  , _SumEmcEt(0.0)
{

}

void BemcEnergySumCalculatorImp::Make()
{
  TowerEnergyDepositList energyList = _bemc->getEnergyList();

  energyList = _cut->Apply(energyList);

  _SumEmcEt = sumEnergyOverBemcTowers(0.4, energyList);

  _DylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, energyList);
}

void BemcEnergySumCalculatorImp::Clear()
{
  _SumEmcEt = 0;
  _DylanPoints = 0;
}

double BemcEnergySumCalculatorImp::sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList)
{
  double ret(0.0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int BemcEnergySumCalculatorImp::numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList)
{
  int ret(0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
}

}
