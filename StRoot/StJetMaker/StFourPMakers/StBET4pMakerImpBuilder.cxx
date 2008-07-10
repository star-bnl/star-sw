// $Id: StBET4pMakerImpBuilder.cxx,v 1.2 2008/07/10 18:48:31 tai Exp $

#include "StBET4pMakerImpBuilder.h"
#include "StBET4pMakerImp.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"

#include "StJetTPCTrackCut.h"
#include "StJetBEMCEnergyCut.h"

#include "CorrectTowerEnergyForTracks.h"

namespace StSpinJet {

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  StJetTPCMuDst*  tpc  = new StJetTPCMuDst(uDstMaker);
  StJetBEMCMuDst* bemc = new StJetBEMCMuDst(uDstMaker, doTowerSwapFix);

  StJetEEMC* eemc;
  if( !useEEMC ) {
    eemc = new StJetEEMCNull();
  } else {
    eemc = new StJetEEMCMuDst(uDstMaker);
  }

  StJetTPCTrackCut*   tpcCut  = new StJetTPCTrackCut();
  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();

  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);


  ret->setUseTPC(useTPC);
  ret->setUseBEMC(useBEMC);

  bemcCut->setUse2003Cuts(use2003Cuts);
  bemcCut->setUse2005Cuts(use2005Cuts);
  tpcCut->setUse2006Cuts(use2006Cuts);

  return ret;
}

}
