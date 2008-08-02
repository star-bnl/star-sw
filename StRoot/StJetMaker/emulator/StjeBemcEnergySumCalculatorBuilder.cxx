// $Id: StjeBemcEnergySumCalculatorBuilder.cxx,v 1.2 2008/08/02 19:23:08 tai Exp $
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

using namespace StJetTowerEnergyCut;

namespace StSpinJet {

StjeBemcEnergySumCalculator* StjeBemcEnergySumCalculatorBuilder::build(bool useBEMC, bool use2003Cuts, bool use2005Cuts, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  if(!useBEMC) return new StjeBemcEnergySumCalculatorNull;

  StjBEMCMuDst* bemc = new StjBEMCMuDst(uDstMaker, doTowerSwapFix);
  StjTowerEnergyListCut* bemcCut = new StjTowerEnergyListCut();
  if(use2003Cuts) bemcCut->addCut(new StjTowerEnergyCut2003BemcTower());
  if(use2005Cuts) bemcCut->addCut(new StjTowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new StjTowerEnergyCutEnergy());
  bemcCut->addCut(new StjTowerEnergyCutBemcStatus());
  bemcCut->addCut(new StjTowerEnergyCutAdc());

  return new StjeBemcEnergySumCalculatorImp(bemc, bemcCut);
}

}
