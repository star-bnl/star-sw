// $Id: StBET4pMakerImpBuilder.cxx,v 1.7 2008/07/13 10:02:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StBET4pMakerImpBuilder.h"
#include "StBET4pMakerImp.h"

#include "StJetTPCMuDst.h"
#include "StJetBEMCMuDst.h"
#include "StJetEEMCMuDst.h"

#include "StJetTPCTrackCut.h"

#include "TrackCutDca.h"
#include "TrackCutDcaPtDependent.h"
#include "TrackCutEta.h"
#include "TrackCutPossibleHitRatio.h"

#include "StJetBEMCEnergyCut.h"

#include "TowerEnergyCut2003BemcTower.h"
#include "TowerEnergyCutBemcWestOnly.h"
#include "TowerEnergyCutEnergy.h"
#include "TowerEnergyCutBemcStatus.h"
#include "TowerEnergyCutAdc.h"

#include "CorrectTowerEnergyForTracks.h"

using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;

namespace StSpinJet {

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StMuDstMaker* uDstMaker, bool doTowerSwapFix)
{
  StJetTPC*  tpc;
  StJetTPCTrackCut* tpcCut  = new StJetTPCTrackCut();
  if( !useTPC ) {
    tpc  = new StJetTPCNull();
  } else {
    tpc  = new StJetTPCMuDst(uDstMaker);
    tpcCut->addCut(new TrackCutDca());
    if(use2006Cuts)  tpcCut->addCut(new TrackCutDcaPtDependent());
    tpcCut->addCut(new TrackCutEta());
    tpcCut->addCut(new TrackCutPossibleHitRatio());
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



  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  return ret;
}

}
