// -*- mode: C++ -*-

//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 25 August 2009
//

#ifndef STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_FRACTION_H
#define STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_FRACTION_H

// STAR
#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

// Local
#include "StjAbstractTowerEnergyCorrectionForTracks.h"

class StjTowerEnergyCorrectionForTracksFraction : public StjAbstractTowerEnergyCorrectionForTracks {
public:
  StjTowerEnergyCorrectionForTracksFraction(float fraction) : mFraction(fraction) {}
  virtual ~StjTowerEnergyCorrectionForTracksFraction() {}

  StjTowerEnergyList Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList);

  float fraction() const { return mFraction; }
  void setFraction(float fraction) { mFraction = fraction; }

private:
  float mFraction;

  ClassDef(StjTowerEnergyCorrectionForTracksFraction,0);
};

#endif // STJ_TOWER_ENERGY_CORRECTION_FOR_TRACKS_FRACTION_H
