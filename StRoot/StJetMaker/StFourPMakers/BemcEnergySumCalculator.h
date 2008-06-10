// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculator.h,v 1.1 2008/06/10 05:40:39 tai Exp $
#ifndef BEMCENERGYSUMCALCULATOR_H
#define BEMCENERGYSUMCALCULATOR_H

namespace StSpinJet {

class CollectEnergyDepositsFromBEMC;

class BemcEnergySumCalculator {

public:
  BemcEnergySumCalculator(CollectEnergyDepositsFromBEMC *collectEnergyDepositsFromBEMC);
  virtual ~BemcEnergySumCalculator() { }

private:

  CollectEnergyDepositsFromBEMC *_collectEnergyDepositsFromBEMC;

};

}

#endif // BEMCENERGYSUMCALCULATOR_H
