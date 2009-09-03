// $Id: StBET4pMakerImpBuilder.cxx,v 1.8 2009/09/03 23:36:14 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StBET4pMakerImpBuilder.h"
#include "StBET4pMakerImp.h"

#include "StjTPCMuDst.h"
#include "StjBEMCMuDst.h"
#include "StjEEMCMuDst.h"
#include "StjTPCNull.h"
#include "StjBEMCNull.h"
#include "StjEEMCNull.h"

#include "StjBEMCSockeVariation.h"

#include "StjTPCTree.h"
#include "StjBEMCTree.h"

#include "StjTrackListCut.h"

#include "StjTrackCutDca.h"
#include "StjTrackCutDcaPtDependent.h"
#include "StjTrackCutEta.h"
#include "StjTrackCutPossibleHitRatio.h"

#include "StjTowerEnergyListCut.h"

#include "StjTowerEnergyCut2003BemcTower.h"
#include "StjTowerEnergyCutBemcWestOnly.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutAdc.h"

#include "StjTowerEnergyListVariation.h"
#include "StjTowerEnergyVariationEnergy.h"

#include "StjAbstractTowerEnergyCorrectionForTracks.h"

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       bool useBEMCEnergyVariation, double bemcEnergyVariationRatio,
					       StMuDstMaker* uDstMaker, bool doTowerSwapFix,
					       StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks)
{
  StjTPC*  tpc;
  StjTrackListCut* tpcCut  = new StjTrackListCut();
  if( !useTPC ) {
    tpc  = new StjTPCNull();
  } else {
    tpc  = new StjTPCMuDst(uDstMaker);
    tpcCut->addCut(new StjTrackCutDca());
    if(use2006Cuts)  tpcCut->addCut(new StjTrackCutDcaPtDependent());
    tpcCut->addCut(new StjTrackCutEta());
    tpcCut->addCut(new StjTrackCutPossibleHitRatio());
  }

  StjBEMC* bemc;
  StjTowerEnergyListCut* bemcCut = new StjTowerEnergyListCut();
  if( !useBEMC ) {
    bemc = new StjBEMCNull();
  } else {
    bemc = new StjBEMCMuDst(uDstMaker, doTowerSwapFix);

    if(use2003Cuts) bemcCut->addCut(new StjTowerEnergyCut2003BemcTower());
    if(use2005Cuts) bemcCut->addCut(new StjTowerEnergyCutBemcWestOnly());
    bemcCut->addCut(new StjTowerEnergyCutEnergy());
    bemcCut->addCut(new StjTowerEnergyCutBemcStatus());
    bemcCut->addCut(new StjTowerEnergyCutAdc());
  }

  if( useBEMCEnergyVariation ) {
    StjTowerEnergyListVariation* variation = new StjTowerEnergyListVariation;
    variation->addVariation(new StjTowerEnergyVariationEnergy(bemcEnergyVariationRatio));
    bemc = new StjBEMCSockeVariation(bemc, variation);
  }

  StjEEMC* eemc;
  if( !useEEMC ) {
    eemc = new StjEEMCNull();
  } else {
    eemc = new StjEEMCMuDst(uDstMaker);
  }

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  return ret;
}

