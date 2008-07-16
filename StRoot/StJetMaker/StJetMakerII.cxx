// $Id: StJetMakerII.cxx,v 1.1 2008/07/16 21:53:20 tai Exp $
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
  StJetTPCTrackCut* tpcCut  = new StJetTPCTrackCut();
  tpcCut->addCut(new TrackCutDca());
  tpcCut->addCut(new TrackCutEta());
  tpcCut->addCut(new TrackCutPossibleHitRatio());

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
