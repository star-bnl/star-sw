// $Id: BemcEnergySumCalculatorBuilder.cxx,v 1.5 2008/07/13 10:02:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "BemcEnergySumCalculatorBuilder.h"
#include "BemcEnergySumCalculator.h"

#include "StJetBEMCMuDst.h"
#include "StJetBEMCEnergyCut.h"

#include "TowerEnergyCut2003BemcTower.h"
#include "TowerEnergyCutBemcWestOnly.h"
#include "TowerEnergyCutEnergy.h"
#include "TowerEnergyCutBemcStatus.h"
#include "TowerEnergyCutAdc.h"

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
