// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculator.h,v 1.5 2008/07/10 09:36:00 tai Exp $
#ifndef BEMCENERGYSUMCALCULATOR_H
#define BEMCENERGYSUMCALCULATOR_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetBEMC;
class StJetBEMCEnergyCut;

class BemcEnergySumCalculator {

public:

  BemcEnergySumCalculator() { }
  virtual ~BemcEnergySumCalculator() { }

  virtual void Init() { }
  virtual void Make() { }
  virtual void Clear() { }

  virtual int nDylanPoints() const = 0;
  virtual double sumEmcEt() const = 0;

private:

};

class BemcEnergySumCalculatorNull : public BemcEnergySumCalculator {

public:

  BemcEnergySumCalculatorNull() { }
  virtual ~BemcEnergySumCalculatorNull() { }

  void Init() { }
  void Make() { }
  void Clear() { }

  int nDylanPoints() const { return 0; } 
  double sumEmcEt() const { return 0; } 

private:

};

class BemcEnergySumCalculatorImp : public BemcEnergySumCalculator {

public:

  BemcEnergySumCalculatorImp(StJetBEMC* bemc, StJetBEMCEnergyCut* cut);
  virtual ~BemcEnergySumCalculatorImp() { }

  void Init();
  void Make();
  void Clear();

  int nDylanPoints() const { return _DylanPoints; }
  double sumEmcEt() const { return _SumEmcEt; }

private:

  double sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList);

  int numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList);

  StJetBEMC* _bemc;
  StJetBEMCEnergyCut* _cut;

  int _DylanPoints;
  double _SumEmcEt;

};

}

#endif // BEMCENERGYSUMCALCULATOR_H
