// $Id: StJetMakerII.cxx,v 1.14 2008/08/03 00:26:17 tai Exp $
#include "StJetMakerII.h"

#include <StJetFinder/StJetPars.h>

#include <StBET4pMakerImp.h>
#include <StBET4pMakerImpBuilder.h>

#include "StjTPCTree.h"
#include "StjBEMCTree.h"
#include "StjEEMC.h"

#include "StjTreeEntryMaker.h"
#include "StjTreeEntryCoordinator.h"

#include "StjTrackListCut.h"
#include "StjTrackCutDca.h"
#include "StjTrackCutDcaPtDependent.h"
#include "StjTrackCutEta.h"
#include "StjTrackCutPossibleHitRatio.h"
#include "StjTrackCutNHits.h"
#include "StjTrackCutFlag.h"

#include "StjTowerEnergyListCut.h"
#include "StjTowerEnergyCut2003BemcTower.h"
#include "StjTowerEnergyCutBemcWestOnly.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutAdc.h"

#include "StjTowerEnergyPrint.h"
#include "StjJetPrint.h"
#include "StjTrackPrint.h"
#include "StjFourVecPrint.h"

#include "StjFourVecListCut.h"
#include "StjFourVecCutPt.h"

#include "StjTrackList.h"
#include "StjTowerEnergyList.h"

#include "StjJetList.h"

#include "StjJetListCut.h"

#include "StjJetCutPt.h"
#include "StjJetCutEta.h"
#include "StjJetCutNFourVecs.h"

#include "StjJetListWriter.h"

#include <StjTowerEnergyVariation.h>

#include <StjTowerEnergyCorrectionForTracks.h>

#include "StjRunJetFinder.h"

#include <StJetFinder/StConePars.h>

#include <TTree.h>

#include <string>
#include <list>
#include <vector>

using namespace std;

ClassImp(StJetMakerII)
  
StJetMakerII::StJetMakerII(const Char_t *name, TDirectory* file, StjTreeEntryMaker* entryMaker) 
  : StMaker(name)
  , _file(file)
  , _entryMaker(entryMaker)
  , _dataSource(0)
{

}

StJetMakerII::~StJetMakerII()
{

}

Int_t StJetMakerII::Init() 
{
  StjTreeEntryCoordinator* coord = _entryMaker->coordinator();
  TTree *treeTpc = dynamic_cast<TTree*>(coord->file()->Get("tpcTracks"));
  _tpc = new StjTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

  TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
  _bemc = new StjBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

  _tpcCut1  = new StjTrackListCut();
  _tpcCut1->addCut(new StjTrackCutDca(3.0));
  _tpcCut1->addCut(new StjTrackCutEta(-2.0, 2.0));
  _tpcCut1->addCut(new StjTrackCutPossibleHitRatio(0.51));

  _bemcCut1 = new StjTowerEnergyListCut();
  _bemcCut1->addCut(new StjTowerEnergyCutBemcWestOnly());
  _bemcCut1->addCut(new StjTowerEnergyCutEnergy(0.0));
  _bemcCut1->addCut(new StjTowerEnergyCutBemcStatus(1));
  _bemcCut1->addCut(new StjTowerEnergyCutAdc(0, 2.0));

  _towerEnergyCorrectionForTracks = new StjTowerEnergyCorrectionForTracks();

  _tpcCut2  = new StjTrackListCut();
  _tpcCut2->addCut(new StjTrackCutFlag(0));
  _tpcCut2->addCut(new StjTrackCutNHits(20));

  _bemcCut2 = new StjTowerEnergyListCut();
  _bemcCut2->addCut(new StjTowerEnergyCutEnergy(0.0));

  _energyVariationNull    = new StjTowerEnergyVariation(0);
  _energyVariationPlus5   = new StjTowerEnergyVariation(0.05);
  _energyVariationMinus5  = new StjTowerEnergyVariation(-0.05);
  _energyVariationPlus10  = new StjTowerEnergyVariation(0.1);
  _energyVariationMinus10 = new StjTowerEnergyVariation(-0.1);

  _fourCut = new StjFourVecListCut;
  _fourCut->addCut(new StjFourVecCutPt(0.2));

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
  _jetFinder = new StjRunJetFinder(cpars);
  _jetFinder->Init();

  _jetCut = new StjJetListCut();
  _jetCut->addCut(new StjJetCutPt(5.0));

  _jetTreeWriter0 = new StjJetListWriter("jets", "four", _file);
  _jetTreeWriterP5 = new StjJetListWriter("jetsP5", "fourP5", _file);
  _jetTreeWriterM5 = new StjJetListWriter("jetsM5", "fourM5", _file);
  _jetTreeWriterP10 = new StjJetListWriter("jetsP10", "fourP10", _file);
  _jetTreeWriterM10 = new StjJetListWriter("jetsM10", "fourM10", _file);

  return kStOk;
}

Int_t StJetMakerII::Make()
{
  StjTrackList trackList = _tpc->getTrackList();
  StjTowerEnergyList energyList = _bemc->getEnergyList();

  StjTowerEnergyPrint printEnergy;

  trackList = (*_tpcCut1)(trackList);
  energyList = (*_bemcCut1)(energyList);

  //  printEnergy(energyList);

  StjTowerEnergyList energyList0 = (*_energyVariationNull)(energyList);
  StjTowerEnergyList energyListP5 = (*_energyVariationPlus5)(energyList);
  StjTowerEnergyList energyListM5 = (*_energyVariationMinus5)(energyList);
  StjTowerEnergyList energyListP10 = (*_energyVariationPlus10)(energyList);
  StjTowerEnergyList energyListM10 = (*_energyVariationMinus10)(energyList);

  energyList0 = (*_towerEnergyCorrectionForTracks)(energyList0, trackList);
  energyListP5 = (*_towerEnergyCorrectionForTracks)(energyListP5, trackList);
  energyListM5 = (*_towerEnergyCorrectionForTracks)(energyListM5, trackList);
  energyListP10 = (*_towerEnergyCorrectionForTracks)(energyListP10, trackList);
  energyListM10 = (*_towerEnergyCorrectionForTracks)(energyListM10, trackList);

  //  printEnergy(energyList0);

  trackList = (*_tpcCut2)(trackList);

  energyList0 = (*_bemcCut2)(energyList0);
  energyListP5 = (*_bemcCut2)(energyListP5);
  energyListM5 = (*_bemcCut2)(energyListM5);
  energyListP10 = (*_bemcCut2)(energyListP10);
  energyListM10 = (*_bemcCut2)(energyListM10);

  //  StjTrackPrint printTrack;
  //  printTrack(trackList);

  StjFourVecList fourList0 = _toP4(trackList, energyList0);
  StjFourVecList fourListP5 = _toP4(trackList, energyListP5);
  StjFourVecList fourListM5 = _toP4(trackList, energyListM5);
  StjFourVecList fourListP10 = _toP4(trackList, energyListP10);
  StjFourVecList fourListM10 = _toP4(trackList, energyListM10);


  fourList0 = (*_fourCut)(fourList0);
  fourListP5 = (*_fourCut)(fourListP5);
  fourListM5 = (*_fourCut)(fourListM5);
  fourListP10 = (*_fourCut)(fourListP10);
  fourListM10 = (*_fourCut)(fourListM10);

  StjFourVecPrint printFour;
  //  printFour(fourList0);

  StjJetList jetList0 = (*_jetFinder)(fourList0);
  StjJetList jetListP5 = (*_jetFinder)(fourListP5);
  StjJetList jetListM5 = (*_jetFinder)(fourListM5);
  StjJetList jetListP10 = (*_jetFinder)(fourListP10);
  StjJetList jetListM10 = (*_jetFinder)(fourListM10);

  StjJetPrint jetprint;
  //  jetprint(jetList0);
  //  jetprint(jetListM5);

  jetList0 = (*_jetCut)(jetList0);
  jetListP5 = (*_jetCut)(jetListP5);
  jetListM5 = (*_jetCut)(jetListM5);
  jetListP10 = (*_jetCut)(jetListP10);
  jetListM10 = (*_jetCut)(jetListM10);


  //  jetprint(jetList0);
  //  jetprint(jetListM5);

  _jetTreeWriter0->Fill(jetList0, fourList0);
  _jetTreeWriterP5->Fill(jetListP5, fourListP5);
  _jetTreeWriterM5->Fill(jetListM5, fourListM5);
  _jetTreeWriterP10->Fill(jetListP10, fourListP10);
  _jetTreeWriterM10->Fill(jetListM10, fourListM10);

  return kStOk;
}

Int_t StJetMakerII::Finish()
{
  _jetTreeWriter0->Finish();
  _jetTreeWriterP5->Finish();
  _jetTreeWriterM5->Finish();
  _jetTreeWriterP10->Finish();
  _jetTreeWriterM10->Finish();

  return kStOK;
}
