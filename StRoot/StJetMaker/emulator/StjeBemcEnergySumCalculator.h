// -*- mode: c++;-*-
// $Id: StjeBemcEnergySumCalculator.h,v 1.7 2008/11/04 08:08:11 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEBEMCENERGYSUMCALCULATOR_H
#define STJEBEMCENERGYSUMCALCULATOR_H

#include "StjTowerEnergyList.h"

class StjBEMC;
class StjTowerEnergyListCut;

class StjeBemcEnergySumCalculator {

public:

  StjeBemcEnergySumCalculator() { }
  virtual ~StjeBemcEnergySumCalculator() { }

  virtual void Init() { }
  virtual void Make() { }
  virtual void Clear() { }

  virtual int nDylanPoints() const = 0;
  virtual double sumEmcEt() const = 0;

private:

};

class StjeBemcEnergySumCalculatorNull : public StjeBemcEnergySumCalculator {

public:

  StjeBemcEnergySumCalculatorNull() { }
  virtual ~StjeBemcEnergySumCalculatorNull() { }

  void Init() { }
  void Make() { }
  void Clear() { }

  int nDylanPoints() const { return 0; } 
  double sumEmcEt() const { return 0; } 

private:

};

class StjeBemcEnergySumCalculatorImp : public StjeBemcEnergySumCalculator {

public:

  StjeBemcEnergySumCalculatorImp(StjBEMC* bemc, StjTowerEnergyListCut* cut);
  virtual ~StjeBemcEnergySumCalculatorImp() { }

  void Init();
  void Make();
  void Clear();

  int nDylanPoints() const { return _DylanPoints; }
  double sumEmcEt() const { return _SumEmcEt; }

private:

  double sumEnergyOverBemcTowers(double minE, const StjTowerEnergyList &energyDepositList);

  int numberOfBemcTowersWithEnergyAbove(double minE, const StjTowerEnergyList &energyDepositList);

  StjBEMC* _bemc;
  StjTowerEnergyListCut* _cut;

  int _DylanPoints;
  double _SumEmcEt;

};

#endif // STJEBEMCENERGYSUMCALCULATOR_H
