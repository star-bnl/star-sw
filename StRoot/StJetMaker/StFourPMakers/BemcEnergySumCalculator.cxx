// $Id: BemcEnergySumCalculator.cxx,v 1.3 2008/07/09 23:53:27 tai Exp $
#include "BemcEnergySumCalculator.h"

#include "StJetBEMC.h"
#include "StJetBEMCEnergyCut.h"

namespace StSpinJet {

BemcEnergySumCalculator::BemcEnergySumCalculator(StJetBEMC* bemc, StJetBEMCEnergyCut* cut)
  : _bemc(bemc)
  , _cut(cut)
  , _DylanPoints(0)
  , _SumEmcEt(0.0)
{

}

void BemcEnergySumCalculator::Make()
{
  TowerEnergyDepositList energyList = _bemc->getEnergyList();

  energyList = _cut->Apply(energyList);

  _SumEmcEt = sumEnergyOverBemcTowers(0.4, energyList);

  _DylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, energyList);
}

void BemcEnergySumCalculator::Clear()
{
  _SumEmcEt = 0;
  _DylanPoints = 0;
}

double BemcEnergySumCalculator::sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList)
{
  double ret(0.0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int BemcEnergySumCalculator::numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList)
{
  int ret(0);

  for(TowerEnergyDepositList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
}

}
