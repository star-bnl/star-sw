// $Id: StjeBemcEnergySumCalculator.cxx,v 1.3 2008/08/03 00:26:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeBemcEnergySumCalculator.h"

#include "StjBEMC.h"
#include "StjTowerEnergyListCut.h"

StjeBemcEnergySumCalculatorImp::StjeBemcEnergySumCalculatorImp(StjBEMC* bemc, StjTowerEnergyListCut* cut)
  : _bemc(bemc)
  , _cut(cut)
  , _DylanPoints(0)
  , _SumEmcEt(0.0)
{

}
void StjeBemcEnergySumCalculatorImp::Init()
{
  _bemc->Init();
}

void StjeBemcEnergySumCalculatorImp::Make()
{
  StjTowerEnergyList energyList = _bemc->getEnergyList();

  energyList = (*_cut)(energyList);

  _SumEmcEt = sumEnergyOverBemcTowers(0.4, energyList);

  _DylanPoints = numberOfBemcTowersWithEnergyAbove(0.4, energyList);
}

void StjeBemcEnergySumCalculatorImp::Clear()
{
  _SumEmcEt = 0;
  _DylanPoints = 0;
}

double StjeBemcEnergySumCalculatorImp::sumEnergyOverBemcTowers(double minE, const StjTowerEnergyList &energyDepositList)
{
  double ret(0.0);

  for(StjTowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret += (*it).energy;

  return ret;
}

int StjeBemcEnergySumCalculatorImp::numberOfBemcTowersWithEnergyAbove(double minE, const StjTowerEnergyList &energyDepositList)
{
  int ret(0);

  for(StjTowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it)
    if((*it).energy > minE) ret ++;

  return ret;
}
