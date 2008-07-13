// $Id: StBET4pMakerImpBuilder.cxx,v 1.4 2008/07/13 02:38:50 tai Exp $

#include "StBET4pMakerImpBuilder.h"
#include "StBET4pMakerImp.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"

#include "StJetTPCTrackCut.h"
#include "StJetBEMCEnergyCut.h"

#include "CorrectTowerEnergyForTracks.h"

using namespace StJetTowerEnergyCut;

namespace StSpinJet {

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  StJetTPC*  tpc;
  if( !useTPC ) {
    tpc  = new StJetTPCNull();
  } else {
    tpc  = new StJetTPCMuDst(uDstMaker);
  }

  StJetBEMC* bemc;
  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();
  if( !useBEMC ) {
    bemc = new StJetBEMCNull();
  } else {
    bemc = new StJetBEMCMuDst(uDstMaker, doTowerSwapFix);

    if(use2003Cuts) bemcCut->addCut(new TowerEnergyCut2003BemcTower());
    if(use2005Cuts) bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
    bemcCut->addCut(new TowerEnergyCutEnergy());
    bemcCut->addCut(new TowerEnergyCutBemcStatus());
    bemcCut->addCut(new TowerEnergyCutAdc());
  }

  StJetEEMC* eemc;
  if( !useEEMC ) {
    eemc = new StJetEEMCNull();
  } else {
    eemc = new StJetEEMCMuDst(uDstMaker);
  }

  StJetTPCTrackCut*   tpcCut  = new StJetTPCTrackCut();


  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  tpcCut->setUse2006Cuts(use2006Cuts);

  return ret;
}

}
