// $Id: StJetMakerII.cxx,v 1.2 2008/07/16 22:28:26 tai Exp $
#include "StJetMakerII.h"

#include <StJetFinder/StJetPars.h>

#include <StBET4pMakerImp.h>
#include <StBET4pMakerImpBuilder.h>

#include "StJetTPCTree.h"
#include "StJetBEMCTree.h"
#include "StJetEEMC.h"

#include "StJetTreeEntryMaker.h"
#include "StJetTreeEntryCoordinator.h"

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

#include <StJetTowerEnergyVariation.h>

#include <CorrectTowerEnergyForTracks.h>

#include <TTree.h>

#include <string>
#include <list>
#include <vector>

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;

ClassImp(StJetMakerII)
  
StJetMakerII::StJetMakerII(const Char_t *name, StJetTreeEntryMaker* entryMaker) 
  : StMaker(name)
  , _entryMaker(entryMaker)
  , _dataSource(0)
{

}

StJetMakerII::~StJetMakerII()
{

}

Int_t StJetMakerII::Init() 
{
  StJetTreeEntryCoordinator* coord = _entryMaker->coordinator();
  TTree *treeTpc = dynamic_cast<TTree*>(coord->file()->Get("tpcTracks"));
  StJetTPC* tpc = new StJetTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

  TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
  StJetBEMC* bemc = new StJetBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

  StJetTPCTrackCut* tpcCut  = new StJetTPCTrackCut();
  tpcCut->addCut(new TrackCutDca());
  tpcCut->addCut(new TrackCutEta());
  tpcCut->addCut(new TrackCutPossibleHitRatio());

  StJetBEMCEnergyCut* bemcCut = new StJetBEMCEnergyCut();
  bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  bemcCut->addCut(new TowerEnergyCutEnergy());
  bemcCut->addCut(new TowerEnergyCutBemcStatus());
  bemcCut->addCut(new TowerEnergyCutAdc());

  CorrectTowerEnergyForTracks* correctTowerEnergyForTracks = new CorrectTowerEnergyForTracks();

  return kStOk;
}

Int_t StJetMakerII::Make()
{
  return kStOk;
}

Int_t StJetMakerII::Finish()
{
  return kStOK;
}
