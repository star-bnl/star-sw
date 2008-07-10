// $Id: BemcEnergySumCalculator.cxx,v 1.6 2008/07/10 20:15:16 tai Exp $
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
void BemcEnergySumCalculatorImp::Init()
{
  _bemc->Init();
}

void BemcEnergySumCalculatorImp::Make()
{
  TowerEnergyList energyList = _bemc->getEnergyList();

  energyList = _cut->Apply(energyList);

  _SumEmcEt = sumEnergyOverBemcTowers(0.4, energyList);

  _DylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, energyList);
}

void BemcEnergySumCalculatorImp::Clear()
{
  _SumEmcEt = 0;
  _DylanPoints = 0;
}

double BemcEnergySumCalculatorImp::sumEnergyOverBemcTowers(double minE, const TowerEnergyList &energyDepositList)
{
  double ret(0.0);

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int BemcEnergySumCalculatorImp::numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyList &energyDepositList)
{
  int ret(0);

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
}

}
