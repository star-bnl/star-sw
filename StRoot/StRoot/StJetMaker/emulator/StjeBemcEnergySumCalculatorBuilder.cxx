// $Id: StjeBemcEnergySumCalculatorBuilder.cxx,v 1.4 2010/05/30 07:10:04 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjeBemcEnergySumCalculatorBuilder.h"
#include "StjeBemcEnergySumCalculator.h"

#include "StjBEMCMuDst.h"
#include "StjTowerEnergyListCut.h"

#include "StjTowerEnergyCut2003BemcTower.h"
#include "StjTowerEnergyCutBemcWestOnly.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutAdc.h"

StjeBemcEnergySumCalculator* StjeBemcEnergySumCalculatorBuilder::build(bool useBEMC, bool use2003Cuts, bool use2005Cuts, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  if(!useBEMC) return new StjeBemcEnergySumCalculatorNull;

  StjBEMCMuDst* bemc = new StjBEMCMuDst(doTowerSwapFix);
  StjTowerEnergyListCut* bemcCut = new StjTowerEnergyListCut();
  if(use2003Cuts) bemcCut->addCut(new StjTowerEnergyCut2003BemcTower());
  if(use2005Cuts) bemcCut->addCut(new StjTowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new StjTowerEnergyCutEnergy());
  bemcCut->addCut(new StjTowerEnergyCutBemcStatus());
  bemcCut->addCut(new StjTowerEnergyCutAdc());

  return new StjeBemcEnergySumCalculatorImp(bemc, bemcCut);
}
