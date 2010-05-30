//
// Pibero Djawotho <pibero@tamu.edu>
// Texas A&M University
// 25 August 2009
//
// Adapted from Tai Sakuma's StjTowerEnergyCorrectionForTracks class
//

#include "StjTowerEnergyCorrectionForTracksMip.h"

ClassImp(StjTowerEnergyCorrectionForTracksMip);

StjTowerEnergyList StjTowerEnergyCorrectionForTracksMip::Do(const StjTowerEnergyList& energyDepositList, const StjTrackList& trackList)
{
  StjTowerEnergyList elist;
  // Tower loop
  for (StjTowerEnergyList::const_iterator iTower = energyDepositList.begin(); iTower != energyDepositList.end(); ++iTower) {
    StjTowerEnergy tower = *iTower;
    float eta = tower.towerEta;
    // Track loop
    for (StjTrackList::const_iterator iTrack = trackList.begin(); iTrack != trackList.end(); ++iTrack) {
      const StjTrack& track = *iTrack;
      // Does track extrapolate to tower?
      if (track.exitDetectorId == tower.detectorId && track.exitTowerId == tower.towerId) {
	// You betcha. Do a quick correction for hadronic MIP energy deposition:
	tower.energy -= 0.261*(1+0.056*eta*eta)*cosh(eta);
      }
    } // End track loop
    // Drop towers without energy
    if (tower.energy > 0) elist.push_back(tower);
  } // End tower loop
  return elist;
}
