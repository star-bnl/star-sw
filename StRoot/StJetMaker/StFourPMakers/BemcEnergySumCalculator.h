// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculator.h,v 1.2 2008/06/10 06:35:39 tai Exp $
#ifndef BEMCENERGYSUMCALCULATOR_H
#define BEMCENERGYSUMCALCULATOR_H

#include "CollectEnergyDepositsFromBEMC.h"

namespace StSpinJet {

class BemcEnergySumCalculator {

public:
  BemcEnergySumCalculator(CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC);
  virtual ~BemcEnergySumCalculator() { }

  void Make();

  void Clear();

  int nDylanPoints() const { return mDylanPoints; }
  double sumEmcEt() const { return mSumEmcEt; }

private:

  CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;

  double sumEnergyOverBemcTowers(double minE, const TowerEnergyDepositList &energyDepositList);
  int numberOfBemcTowersWithEnergyAbove(double minE, const TowerEnergyDepositList &energyDepositList);

  int mDylanPoints;
  double mSumEmcEt;

};

}

#endif // BEMCENERGYSUMCALCULATOR_H
