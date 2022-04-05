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
    //if (tower.energy > 0) elist.push_back(tower);

    // Artificially prevent this tower from contributing to
    // the jet energy by assigning a really tiny amount (1e-10 GeV)
    // to the tower energy. Otherwise we would lose the information
    // about the position of the tower (eta and phi)
    // if the tower energy was set to zero. This way we retain
    // the tower ADC that contributes to the jet patch transverse energy
    // and helps in identifying jets that could have fired the
    // jet patch trigger.
    if (tower.energy < 0) tower.energy = 1e-10;
    elist.push_back(tower);
  } // End tower loop
  return elist;
}
