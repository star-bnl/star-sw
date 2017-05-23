// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 27 May 2010
//

#ifndef ST_ANA_PARS_H
#define ST_ANA_PARS_H

#include "TObject.h"
#include "StjTowerEnergyCorrectionForTracksNull.h"
#include "StjTrackPtFraction.h"
#include "StjTrackRegion.h"
#include "StjTowerEnergyFraction.h"
#include "StjTowerRegion.h"
#include "StjMCParticleRegion.h"
#include "StjTrackListCut.h"
#include "StjTowerEnergyListCut.h"
#include "StjMCParticleListCut.h"
#include "StJetFinder/StProtoJetListCut.h"

class StAnaPars : public TObject {
public:
  StAnaPars()
    : mCorrectTowerEnergyForTracks(new StjTowerEnergyCorrectionForTracksNull)
    , useTpc(false)
    , useBemc(false)
    , useEemc(false)
    , useFms(false)
    , useMonteCarlo(false)
    , randomSelectorProb(1.0)
    , randomSelectorAt(false)
    , randomSelectorSeed(0)
    , changeTracks(0)
    , changeTowers(0)
  {
  }

  ~StAnaPars() { delete mCorrectTowerEnergyForTracks; }

  void setTowerEnergyCorrection(StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks)
  {
    mCorrectTowerEnergyForTracks = correctTowerEnergyForTracks;
  }

  void setTrackShift(StjAbstractTrack* changeTrks)
//  void setTrackShift(StjAbstractTrack* changeTracks)
  {
  //  mChangeTracks = changeTracks;
    changeTracks = changeTrks;
  }
//  void setTowerShift(StjAbstractTower* changeTowers)
  void setTowerShift(StjAbstractTower* changeTwrs)
  {
//    mChangeTowers = changeTowers;
    changeTowers = changeTwrs;
  }
  void setTrackRegion(StjAbstractTrackRegion* trackRegion)
  {
    mtrackRegion = trackRegion;
  }
  void setTowerRegion(StjAbstractTowerRegion* towerRegion)
  {
    mtowerRegion = towerRegion;
  }
  void setParticleRegion(StjAbstractMCParticleRegion* particleRegion)
  {
    mparticleRegion = particleRegion;
  }
  

  void addTpcCut(StjTrackCut* c) { mTpcCuts.addCut(c); }
  void addBemcCut(StjTowerEnergyCut* c) { mBemcCuts.addCut(c); }
  void addEemcCut(StjTowerEnergyCut* c) { mEemcCuts.addCut(c); }
  void addMcCut(StjMCParticleCut* c) { mMcCuts.addCut(c); }
  void addJetCut(StProtoJetCut* c) { mJetCuts.addCut(c); }

  StjAbstractTowerEnergyCorrectionForTracks& correctTowerEnergyForTracks() { return *mCorrectTowerEnergyForTracks; }
//  StjAbstractTrack& changeTracks() { return *mChangeTracks; }
//  StjAbstractTower& changeTowers() { return *mChangeTowers; }
  StjAbstractTrackRegion& trackRegion() { return *mtrackRegion; }
  StjAbstractTowerRegion& towerRegion() { return *mtowerRegion; }
  StjAbstractMCParticleRegion& particleRegion(){ return *mparticleRegion; }
  StjTrackListCut& tpcCuts() { return mTpcCuts; }
  StjTowerEnergyListCut& bemcCuts() { return mBemcCuts; }
  StjTowerEnergyListCut& eemcCuts() { return mEemcCuts; }
  StjMCParticleListCut& mcCuts() { return mMcCuts; }
  StProtoJetListCut& jetCuts() { return mJetCuts; }

private:
  StjAbstractTowerEnergyCorrectionForTracks* mCorrectTowerEnergyForTracks;
//  StjAbstractTrack* mChangeTracks;
//  StjAbstractTower* mChangeTowers;
  StjAbstractTrackRegion* mtrackRegion;
  StjAbstractTowerRegion* mtowerRegion;
  StjAbstractMCParticleRegion* mparticleRegion;
  StjTrackListCut mTpcCuts;
  StjTowerEnergyListCut mBemcCuts;
  StjTowerEnergyListCut mEemcCuts;
  StjMCParticleListCut mMcCuts;
  StProtoJetListCut mJetCuts;

public:
  bool useTpc;
  bool useBemc;
  bool useEemc;
  bool useFms;
  bool useMonteCarlo;
  double randomSelectorProb;
  double randomSelectorAt;
  unsigned int randomSelectorSeed;
  StjAbstractTrack* changeTracks;
  StjAbstractTower* changeTowers;

  ClassDef(StAnaPars,0);
};

#endif	// ST_ANA_PARS_H
