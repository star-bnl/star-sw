// -*- mode: c++;-*-
// $Id: BemcEnergySumCalculatorBuilder.h,v 1.2 2008/07/13 10:02:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
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
