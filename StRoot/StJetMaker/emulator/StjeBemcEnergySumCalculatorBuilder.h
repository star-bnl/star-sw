// -*- mode: c++;-*-
// $Id: StjeBemcEnergySumCalculatorBuilder.h,v 1.3 2008/08/02 22:43:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEBEMCENERGYSUMCALCULATORBUILDER_H
#define STJEBEMCENERGYSUMCALCULATORBUILDER_H

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

#endif // STJEBEMCENERGYSUMCALCULATORBUILDER_H
