// $Id: StBET4pMakerImpBuilder.cxx,v 1.2 2008/08/02 04:18:31 tai Exp $
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

StBET4pMakerImp* StBET4pMakerImpBuilder::build(bool useTPC, bool useBEMC, bool useEEMC,
					       bool use2003Cuts, bool use2005Cuts, bool use2006Cuts,
					       StJetTreeEntryMaker* maker)
{
  StJetTPC*  tpc;
  StJetTPCTrackCut* tpcCut  = new StJetTPCTrackCut();
  if( !useTPC ) {
    tpc  = new StJetTPCNull();
  } else {
    StJetTreeEntryCoordinator* coord = maker->coordinator();
    TTree *treeTpc = dynamic_cast<TTree*>(coord->file()->Get("tpcTracks"));
    tpc = new StJetTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

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
    StJetTreeEntryCoordinator* coord = maker->coordinator();
    TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
    bemc = new StJetBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

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
    eemc = new StJetEEMCNull();
  }



  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  StBET4pMakerImp* ret = new StBET4pMakerImp(tpc, tpcCut, bemc, bemcCut, correctTowerEnergyForTracks, eemc);

  return ret;
}


}
