// -*- mode:c++ -*-
//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 30 May 2010
//

#ifndef STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_NULL_H
#define STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_NULL_H

#include "StjAbstractTowerEnergyCorrectionForTracks.h"

class StjTowerEnergyCorrectionForTracksNull : public StjAbstractTowerEnergyCorrectionForTracks {
public:
  StjTowerEnergyList Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList)
  {
    return energyDepositList;
  }

private:
  ClassDef(StjTowerEnergyCorrectionForTracksNull,0);
};

#endif	// STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_NULL_H
