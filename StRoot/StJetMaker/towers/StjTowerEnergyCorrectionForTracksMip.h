// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 25 August 2009
//
// Adapted from Tai Sakuma's StjTowerEnergyCorrectionForTracks class
//

#ifndef STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_MIP_H
#define STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_MIP_H

// STAR
#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

// Local
#include "StjAbstractTowerEnergyCorrectionForTracks.h"

class StjTowerEnergyCorrectionForTracksMip : public StjAbstractTowerEnergyCorrectionForTracks {
public:
  StjTowerEnergyCorrectionForTracksMip() {}
  virtual ~StjTowerEnergyCorrectionForTracksMip() {}

  StjTowerEnergyList Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList);

private:
  ClassDef(StjTowerEnergyCorrectionForTracksMip,1);
};

#endif // STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_MIP_H
