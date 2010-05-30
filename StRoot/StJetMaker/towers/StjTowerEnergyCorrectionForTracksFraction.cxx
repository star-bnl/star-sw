//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 25 August 2009
//

#include "StjTowerEnergyCorrectionForTracksFraction.h"

ClassImp(StjTowerEnergyCorrectionForTracksFraction);

StjTowerEnergyList StjTowerEnergyCorrectionForTracksFraction::Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList)
{
  StjTowerEnergyList elist;
  // Tower loop
  for (StjTowerEnergyList::const_iterator iTower = energyDepositList.begin(); iTower != energyDepositList.end(); ++iTower) {
    StjTowerEnergy tower = *iTower;
    // Track loop
    for (StjTrackList::const_iterator iTrack = trackList.begin(); iTrack != trackList.end(); ++iTrack) {
      const StjTrack& track = *iTrack;
      // Does track extrapolate to tower?
      if (track.exitDetectorId == tower.detectorId && track.exitTowerId == tower.towerId) {
	// You betcha. Subtract track momentum from tower energy.
	tower.energy -= mFraction * track.pt * cosh(track.eta);
      }
    } // End track loop
    // Drop towers without energy
    if (tower.energy > 0) elist.push_back(tower);
  } // End tower loop
  return elist;
}
