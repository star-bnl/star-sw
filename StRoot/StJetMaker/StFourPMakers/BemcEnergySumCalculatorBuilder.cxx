// $Id: BemcEnergySumCalculatorBuilder.cxx,v 1.1 2008/07/10 06:47:48 tai Exp $
#include "BemcEnergySumCalculatorBuilder.h"
#include "BemcEnergySumCalculator.h"

#include "StJetBEMCMuDst.h"
#include "StJetBEMCEnergyCut.h"

namespace StSpinJet {

BemcEnergySumCalculator* BemcEnergySumCalculatorBuilder::build(bool useBEMC, bool use2003Cuts, bool use2005Cuts, StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  if(!useBEMC) return new BemcEnergySumCalculatorNull;

  StJetBEMCMuDst* bemc = new StJetBEMCMuDst(uDstMaker, doTowerSwapFix);
  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();
  bemc->Init();
  bemcCut->setUse2003Cuts(use2003Cuts);
  bemcCut->setUse2005Cuts(use2005Cuts);

  return new BemcEnergySumCalculatorImp(bemc, bemcCut);
}

}
