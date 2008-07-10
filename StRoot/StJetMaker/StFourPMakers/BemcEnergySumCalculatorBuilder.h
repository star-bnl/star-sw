// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculatorBuilder.h,v 1.1 2008/07/10 06:47:48 tai Exp $
#ifndef BEMCENERGYSUMCALCULATORBUILDER_H
#define BEMCENERGYSUMCALCULATORBUILDER_H

class StMuDstMaker;

namespace StSpinJet {

class BemcEnergySumCalculator;

class BemcEnergySumCalculatorBuilder {

public:

  BemcEnergySumCalculatorBuilder() { }
  virtual ~BemcEnergySumCalculatorBuilder() { }

  BemcEnergySumCalculator* build(bool useBEMC = true, bool use2003Cuts = false, bool use2005Cuts = false, StMuDstMaker* uDstMaker = 0, bool doTowerSwapFix = true);

private:

};

}

#endif // BEMCENERGYSUMCALCULATORBUILDER_H
