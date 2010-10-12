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
#include "StjTrackListCut.h"
#include "StjTowerEnergyListCut.h"
#include "StJetFinder/StProtoJetListCut.h"

class StAnaPars : public TObject {
public:
  StAnaPars() : mCorrectTowerEnergyForTracks(new StjTowerEnergyCorrectionForTracksNull) {}
  ~StAnaPars() { delete& mCorrectTowerEnergyForTracks; }

  void setTowerEnergyCorrection(StjAbstractTowerEnergyCorrectionForTracks* correctTowerEnergyForTracks)
  {
    mCorrectTowerEnergyForTracks = correctTowerEnergyForTracks;
  }

  void addTpcCut(StjTrackCut* c) { mTpcCuts.addCut(c); }
  void addBemcCut(StjTowerEnergyCut* c) { mBemcCuts.addCut(c); }
  void addEemcCut(StjTowerEnergyCut* c) { mEemcCuts.addCut(c); }
  void addJetCut(StProtoJetCut* c) { mJetCuts.addCut(c); }

  StjAbstractTowerEnergyCorrectionForTracks& correctTowerEnergyForTracks() { return *mCorrectTowerEnergyForTracks; }
  StjTrackListCut& tpcCuts() { return mTpcCuts; }
  StjTowerEnergyListCut& bemcCuts() { return mBemcCuts; }
  StjTowerEnergyListCut& eemcCuts() { return mEemcCuts; }
  StProtoJetListCut& jetCuts() { return mJetCuts; }

private:
  StjAbstractTowerEnergyCorrectionForTracks* mCorrectTowerEnergyForTracks;
  StjTrackListCut mTpcCuts;
  StjTowerEnergyListCut mBemcCuts;
  StjTowerEnergyListCut mEemcCuts;
  StProtoJetListCut mJetCuts;

  ClassDef(StAnaPars,0);
};

#endif	// ST_ANA_PARS_H
