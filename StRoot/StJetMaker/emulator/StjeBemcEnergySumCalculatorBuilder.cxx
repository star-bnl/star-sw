// $Id: StjeBemcEnergySumCalculatorBuilder.cxx,v 1.1 2008/08/02 04:18:36 tai Exp $
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

BemcEnergySumCalculator* BemcEnergySumCalculatorBuilder::build(bool useBEMC, bool use2003Cuts, bool use2005Cuts, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  if(!useBEMC) return new BemcEnergySumCalculatorNull;

  StJetBEMCMuDst* bemc = new StJetBEMCMuDst(uDstMaker, doTowerSwapFix);
  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();
  if(use2003Cuts) bemcCut->addCut(new TowerEnergyCut2003BemcTower());
  if(use2005Cuts) bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  return new BemcEnergySumCalculatorImp(bemc, bemcCut);
}

}
