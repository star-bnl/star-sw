// $Id: BemcEnergySumCalculator.cxx,v 1.2 2008/06/10 06:35:39 tai Exp $
#include "BemcEnergySumCalculator.h"

#include "CollectEnergyDepositsFromBEMC.h"

namespace StSpinJet {

BemcEnergySumCalculator::BemcEnergySumCalculator(CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC)
 : _collectEnergyDepositsFromBEMC(collectEnergyDepositsFromBEMC)
  , mDylanPoints(0)
  , mSumEmcEt(0.0)
{

}

void BemcEnergySumCalculator::Make()
{
  TowerEnergyDepositList bemcEnergyDepositList = _collectEnergyDepositsFromBEMC->Do();

  mSumEmcEt = sumEnergyOverBemcTowers(0.4, bemcEnergyDepositList);

  mDylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, bemcEnergyDepositList);
}

void BemcEnergySumCalculator::Clear()
{
  mSumEmcEt = 0;
  mDylanPoints = 0;
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
