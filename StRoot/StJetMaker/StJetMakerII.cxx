// $Id: StJetMakerII.cxx,v 1.4 2008/07/17 02:39:51 tai Exp $
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
#include "TrackCutNHits.h"
#include "TrackCutFlag.h"

#include "StJetBEMCEnergyCut.h"
#include "TowerEnergyCut2003BemcTower.h"
#include "TowerEnergyCutBemcWestOnly.h"
#include "TowerEnergyCutEnergy.h"
#include "TowerEnergyCutBemcStatus.h"
#include "TowerEnergyCutAdc.h"

#include "StJetFourListCut.h"
#include "FourCutPt.h"

#include "TrackList.h"
#include "TowerEnergyList.h"

#include <StJetTowerEnergyVariation.h>

#include <CorrectTowerEnergyForTracks.h>

#include "RunJetFinder.h"

#include <StJetFinder/StConePars.h>

#include <TTree.h>

#include <string>
#include <list>
#include <vector>

using namespace std;
using namespace StSpinJet;
using namespace StJetTowerEnergyCut;
using namespace StJetTrackCut;
using namespace StJetFourCut;

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
  _tpc = new StJetTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

  TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
  _bemc = new StJetBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

  _tpcCut1  = new StJetTPCTrackCut();
  _tpcCut1->addCut(new TrackCutDca(3.0));
  _tpcCut1->addCut(new TrackCutEta(-2.0, 2.0));
  _tpcCut1->addCut(new TrackCutPossibleHitRatio(0.51));

  _bemcCut = new StJetBEMCEnergyCut();
  _bemcCut->addCut(new TowerEnergyCutBemcWestOnly());
  _bemcCut->addCut(new TowerEnergyCutEnergy(0.0));
  _bemcCut->addCut(new TowerEnergyCutBemcStatus(1));
  _bemcCut->addCut(new TowerEnergyCutAdc(0, 2.0));

  _towerEnergyCorrectionForTracks = new CorrectTowerEnergyForTracks();

  _tpcCut2  = new StJetTPCTrackCut();
  _tpcCut2->addCut(new TrackCutFlag(0));
  _tpcCut2->addCut(new TrackCutNHits(20));

  _energyVariationNull    = new StJetTowerEnergyVariation(0);
  _energyVariationPlus5   = new StJetTowerEnergyVariation(0.05);
  _energyVariationMinus5  = new StJetTowerEnergyVariation(-0.05);
  _energyVariationPlus10  = new StJetTowerEnergyVariation(0.1);
  _energyVariationMinus10 = new StJetTowerEnergyVariation(-0.1);

  _fourCut = new StJetFourListCut;
  _fourCut->addCut(new FourCutPt(0.2));

  StConePars* cpars = new StConePars();
  cpars->setGridSpacing(56, -1.6, 1.6, 120, -3.141592613589793, 3.141592613589793);
  cpars->setConeRadius(0.4);
  cpars->setSeedEtMin(0.5);
  cpars->setAssocEtMin(0.1);
  cpars->setSplitFraction(0.5);
  cpars->setPerformMinimization(true);
  cpars->setAddMidpoints(true);
  cpars->setRequireStableMidpoints(true);
  cpars->setDoSplitMerge(true);
  cpars->setDebug(false);
  _jetFinder = new RunJetFinder(cpars);
  _jetFinder->Init();

  return kStOk;
}

Int_t StJetMakerII::Make()
{

  TrackList trackList = _tpc->getTrackList();
  TowerEnergyList energyList = _bemc->getEnergyList();

  pair<TrackList, TowerEnergyList> trackAndEnergy = pair<TrackList, TowerEnergyList>(trackList, energyList);

  TObjArray fourList = _toP4(trackAndEnergy);
  fourList.SetOwner(kTRUE);

  return kStOk;
}

Int_t StJetMakerII::Finish()
{
  return kStOK;
}
