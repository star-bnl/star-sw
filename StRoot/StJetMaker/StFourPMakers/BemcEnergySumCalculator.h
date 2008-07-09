// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculator.h,v 1.3 2008/07/09 23:53:28 tai Exp $
#ifndef BEMCENERGYSUMCALCULATOR_H
#define BEMCENERGYSUMCALCULATOR_H

#include "TowerEnergyDeposit.h"

namespace StSpinJet {

class StJetBEMC;
class StJetBEMCEnergyCut;

class BemcEnergySumCalculator {

public:

  BemcEnergySumCalculator(StJetBEMC* bemc, StJetBEMCEnergyCut* cut);
  virtual ~BemcEnergySumCalculator() { }

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
