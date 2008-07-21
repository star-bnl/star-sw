// $Id: CorrectTowerEnergyForTracks.cxx,v 1.1 2008/07/21 17:24:39 tai Exp $
#include "CorrectTowerEnergyForTracks.h"

namespace StSpinJet {

const int CorrectTowerEnergyForTracks::mNOfBemcTowers;

TowerEnergyList CorrectTowerEnergyForTracks::operator()(const TowerEnergyList &energyDepositList, const TrackList& trackList)
 {
   return Do(energyDepositList, trackList);
}

TowerEnergyList CorrectTowerEnergyForTracks::Do(const TowerEnergyList &energyDepositList, const TrackList& trackList)
{
  for (int i = 1; i <= mNOfBemcTowers; ++i) {
    mNtracksOnTower[i] = 0;
  }

  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    countTracksOnBemcTower(*track);
  }

  TowerEnergyList ret;

  for(TowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    TowerEnergy energyDeposit(*it);

    energyDeposit.energy = correctBemcTowerEnergyForTracks_(energyDeposit.energy, energyDeposit.towerId, energyDeposit.towerEta, energyDeposit.towerPhi);

    energyDeposit.energy = (energyDeposit.energy <= 0) ? 0 : energyDeposit.energy;

    ret.push_back(energyDeposit);
  }

  return ret;
}

void CorrectTowerEnergyForTracks::countTracksOnBemcTower(const Track& track)
{
  if(track.exitDetectorId == 9 && track.exitTowerId != 0)
    mNtracksOnTower[track.exitTowerId]++;
}

double CorrectTowerEnergyForTracks::correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId, float eta, float phi)
{
    float theta=2.*atan(exp(-eta));

    //do a quick correction for hadronic MIP eneryg deposition:
    double MipE = 0.261*(1.+0.056*eta*eta)/sin(theta); //GeV

    return energy - mNtracksOnTower[bemcTowerId]*MipE;
}


}
