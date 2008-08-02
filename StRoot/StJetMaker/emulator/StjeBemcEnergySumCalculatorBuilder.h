// -*- mode: c++;-*-
// $Id: StjeBemcEnergySumCalculatorBuilder.h,v 1.2 2008/08/02 19:23:08 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef BEMCENERGYSUMCALCULATORBUILDER_H
#define BEMCENERGYSUMCALCULATORBUILDER_H

class StMuDstMaker;

namespace StSpinJet {

class StjeBemcEnergySumCalculator;

class StjeBemcEnergySumCalculatorBuilder {

public:

  StjeBemcEnergySumCalculatorBuilder() { }
  virtual ~StjeBemcEnergySumCalculatorBuilder() { }

  StjeBemcEnergySumCalculator* build(bool useBEMC = true, bool use2003Cuts = false, bool use2005Cuts = false, StMuDstMaker* uDstMaker = 0, bool doTowerSwapFix = true);

private:

};

}

#endif // BEMCENERGYSUMCALCULATORBUILDER_H
