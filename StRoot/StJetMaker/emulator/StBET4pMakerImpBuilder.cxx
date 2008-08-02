// $Id: StBET4pMakerImpBuilder.cxx,v 1.3 2008/08/02 19:23:06 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StBET4pMakerImpBuilder.h"
#include "StBET4pMakerImp.h"

#include "StjTPCMuDst.h"
#include "StjBEMCMuDst.h"
#include "StjEEMCMuDst.h"

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

#include "StjTowerEnergyCorrectionForTracks.h"

#include "StjTreeEntryCoordinator.h"
#include "StjTreeEntryMaker.h"

#include <TFile.h>
#include <TTree.h>

using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;

namespace StSpinJet {

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StMuDstMaker* uDstMaker, bool doTowerSwapFix)
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

  StjEEMC* eemc;
  if( !useEEMC ) {
    eemc = new StjEEMCNull();
  } else {
    eemc = new StjEEMCMuDst(uDstMaker);
  }



  StjTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks = new StjTowerEnergyCorrectionForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  return ret;
}

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StjTreeEntryMaker* maker)
{
  StjTPC*  tpc;
  StjTrackListCut* tpcCut  = new StjTrackListCut();
  if( !useTPC ) {
    tpc  = new StjTPCNull();
  } else {
    StjTreeEntryCoordinator* coord = maker->coordinator();
    TTree *treeTpc = dynamic_cast<TTree*>(coord->file()->Get("tpcTracks"));
    tpc = new StjTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

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
    StjTreeEntryCoordinator* coord = maker->coordinator();
    TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
    bemc = new StjBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

    if(use2003Cuts) bemcCut->addCut(new StjTowerEnergyCut2003BemcTower());
    if(use2005Cuts) bemcCut->addCut(new StjTowerEnergyCutBemcWestOnly());
    bemcCut->addCut(new StjTowerEnergyCutEnergy());
    bemcCut->addCut(new StjTowerEnergyCutBemcStatus());
    bemcCut->addCut(new StjTowerEnergyCutAdc());
  }

  StjEEMC* eemc;
  if( !useEEMC ) {
    eemc = new StjEEMCNull();
  } else {
    eemc = new StjEEMCNull();
  }



  StjTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks = new StjTowerEnergyCorrectionForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  return ret;
}


}
