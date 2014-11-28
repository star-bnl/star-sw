// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 25 August 2009
//

#ifndef STJ_ABSTRACT_TOWER_ENERGY_CORRECTION_FOR_TRACKS_H
#define STJ_ABSTRACT_TOWER_ENERGY_CORRECTION_FOR_TRACKS_H

// ROOT
#include "TObject.h"

// STAR
#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

class StjAbstractTowerEnergyCorrectionForTracks : public TObject {
public:
  StjAbstractTowerEnergyCorrectionForTracks() {}
  virtual ~StjAbstractTowerEnergyCorrectionForTracks() {}

  StjTowerEnergyList operator()(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList)
  {
    return Do(energyDepositList,trackList);
  }

  virtual StjTowerEnergyList Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList) = 0;

  ClassDef(StjAbstractTowerEnergyCorrectionForTracks,0);
};

#endif // STJ_ABSTRACT_TOWER_ENERGY_CORRECTION_FOR_TRACKS_H
