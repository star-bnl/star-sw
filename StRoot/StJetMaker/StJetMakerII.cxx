// $Id: StJetMakerII.cxx,v 1.9 2008/07/18 19:20:08 tai Exp $
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

#include "StJetTowerEnergyPrint.h"
#include "StJetJetPrint.h"
#include "StJetTrackPrint.h"
#include "StJetFourVecPrint.h"

#include "StJetJetListWriter.h"

#include "StJetFourVecListCut.h"
#include "FourVecCutPt.h"

#include "TrackList.h"
#include "TowerEnergyList.h"

#include "JetList.h"

#include "StJetJetListCut.h"

#include "JetCutPt.h"
#include "JetCutEta.h"
#include "JetCutNFourVecs.h"

#include "StJetJetListWriter.h"

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
using namespace StJetFourVecCut;
using namespace StJetJetCut;

ClassImp(StJetMakerII)
  
StJetMakerII::StJetMakerII(const Char_t *name, TDirectory* file, StJetTreeEntryMaker* entryMaker) 
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
  StJetTreeEntryCoordinator* coord = _entryMaker->coordinator();
  TTree *treeTpc = dynamic_cast<TTree*>(coord->file()->Get("tpcTracks"));
  _tpc = new StJetTPCTree(treeTpc, coord->indexMajor(), coord->indexMinor());

  TTree *treeBemc = dynamic_cast<TTree*>(coord->file()->Get("bemcTowers"));
  _bemc = new StJetBEMCTree(treeBemc, coord->indexMajor(), coord->indexMinor());

  _tpcCut1  = new StJetTPCTrackCut();
  _tpcCut1->addCut(new TrackCutDca(3.0));
  _tpcCut1->addCut(new TrackCutEta(-2.0, 2.0));
  _tpcCut1->addCut(new TrackCutPossibleHitRatio(0.51));

  _bemcCut1 = new StJetBEMCEnergyCut();
  _bemcCut1->addCut(new TowerEnergyCutBemcWestOnly());
  _bemcCut1->addCut(new TowerEnergyCutEnergy(0.0));
  _bemcCut1->addCut(new TowerEnergyCutBemcStatus(1));
  _bemcCut1->addCut(new TowerEnergyCutAdc(0, 2.0));

  _towerEnergyCorrectionForTracks = new CorrectTowerEnergyForTracks();

  _tpcCut2  = new StJetTPCTrackCut();
  _tpcCut2->addCut(new TrackCutFlag(0));
  _tpcCut2->addCut(new TrackCutNHits(20));

  _bemcCut2 = new StJetBEMCEnergyCut();
  _bemcCut2->addCut(new TowerEnergyCutEnergy(0.0));

  _energyVariationNull    = new StJetTowerEnergyVariation(0);
  _energyVariationPlus5   = new StJetTowerEnergyVariation(0.05);
  _energyVariationMinus5  = new StJetTowerEnergyVariation(-0.05);
  _energyVariationPlus10  = new StJetTowerEnergyVariation(0.1);
  _energyVariationMinus10 = new StJetTowerEnergyVariation(-0.1);

  _fourCut = new StJetFourVecListCut;
  _fourCut->addCut(new FourVecCutPt(0.2));

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

  _jetCut = new StJetJetListCut();
  _jetCut->addCut(new JetCutPt(5.0));

  _jetTreeWriter0 = new StJetJetListWriter("jets", "four", _file);
  _jetTreeWriterP5 = new StJetJetListWriter("jetsP5", "fourP5", _file);
  _jetTreeWriterM5 = new StJetJetListWriter("jetsM5", "fourM5", _file);
  _jetTreeWriterP10 = new StJetJetListWriter("jetsP10", "fourP10", _file);
  _jetTreeWriterM10 = new StJetJetListWriter("jetsM10", "fourM10", _file);

  return kStOk;
}

Int_t StJetMakerII::Make()
{
  TrackList trackList = _tpc->getTrackList();
  TowerEnergyList energyList = _bemc->getEnergyList();

  StJetTowerEnergyPrint printEnergy;

  trackList = (*_tpcCut1)(trackList);
  energyList = (*_bemcCut1)(energyList);

  //  printEnergy(energyList);

  TowerEnergyList energyList0 = (*_energyVariationNull)(energyList);
  TowerEnergyList energyListP5 = (*_energyVariationPlus5)(energyList);
  TowerEnergyList energyListM5 = (*_energyVariationMinus5)(energyList);
  TowerEnergyList energyListP10 = (*_energyVariationPlus10)(energyList);
  TowerEnergyList energyListM10 = (*_energyVariationMinus10)(energyList);

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

  //  StJetTrackPrint printTrack;
  //  printTrack(trackList);

  FourVecList fourList0 = _toP4(trackList, energyList0);
  FourVecList fourListP5 = _toP4(trackList, energyListP5);
  FourVecList fourListM5 = _toP4(trackList, energyListM5);
  FourVecList fourListP10 = _toP4(trackList, energyListP10);
  FourVecList fourListM10 = _toP4(trackList, energyListM10);


  fourList0 = (*_fourCut)(fourList0);
  fourListP5 = (*_fourCut)(fourListP5);
  fourListM5 = (*_fourCut)(fourListM5);
  fourListP10 = (*_fourCut)(fourListP10);
  fourListM10 = (*_fourCut)(fourListM10);

  StJetFourVecPrint printFour;
  //  printFour(fourList0);

  JetList jetList0 = (*_jetFinder)(fourList0);
  JetList jetListP5 = (*_jetFinder)(fourListP5);
  JetList jetListM5 = (*_jetFinder)(fourListM5);
  JetList jetListP10 = (*_jetFinder)(fourListP10);
  JetList jetListM10 = (*_jetFinder)(fourListM10);

  StJetJetPrint jetprint;
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
