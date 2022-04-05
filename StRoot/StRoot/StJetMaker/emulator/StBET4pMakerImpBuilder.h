// -*- mode: c++;-*-
// $Id: StBET4pMakerImpBuilder.h,v 1.9 2010/05/24 17:42:32 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STBET4PMAKERIMPBUILDER_H
#define STBET4PMAKERIMPBUILDER_H

class StMuDstMaker;
class StBET4pMakerImp;
class StjAbstractTowerEnergyCorrectionForTracks;

class StBET4pMakerImpBuilder { 
public:
  StBET4pMakerImpBuilder() { }
  virtual ~StBET4pMakerImpBuilder() { }

  StBET4pMakerImp* build(bool useTPC = true, bool useBEMC = true, bool useEEMC = false, bool use2003Cuts = false, bool use2005Cuts = false, bool use2006Cuts = false, bool use2009Cuts = false, bool useBEMCEnergyVariation = false, double bemcEnergyVariationRatio = 0.05, bool useRandomSelector = false, StMuDstMaker* maker = 0, bool doTowerSwapFix = true, StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks = 0, double randomSelectorProb = 1.0, bool randomSelectorAt = false, unsigned int randomSelectorSeed = 0);
};

#endif // STBET4PMAKERIMPBUILDER_H
