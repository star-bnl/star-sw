// $Id: StjTowerEnergyCorrectionForTracks.cxx,v 1.1 2008/11/27 07:35:25 tai Exp $
#include "StjTowerEnergyCorrectionForTracks.h"

#include <iostream>

ClassImp(StjTowerEnergyCorrectionForTracks)

using namespace std;

const int StjTowerEnergyCorrectionForTracks::mNOfBemcTowers;

StjTowerEnergyList StjTowerEnergyCorrectionForTracks::operator()(const StjTowerEnergyList &energyDepositList, const StjTrackList& trackList)
 {
   return Do(energyDepositList, trackList);
}

StjTowerEnergyList StjTowerEnergyCorrectionForTracks::Do(const StjTowerEnergyList &energyDepositList, const StjTrackList& trackList)
{
  for (int i = 1; i <= mNOfBemcTowers; ++i) {
    mNtracksOnTower[i] = 0;
  }

  for(StjTrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    countTracksOnBemcTower(*track);
  }

  StjTowerEnergyList ret;

  for(StjTowerEnergyList::const_iterator it = energyDepositList.begin(); it != energyDepositList.end(); ++it) {

    StjTowerEnergy energyDeposit(*it);

    energyDeposit.energy = correctBemcTowerEnergyForTracks_(energyDeposit.energy, energyDeposit.towerId, energyDeposit.towerEta, energyDeposit.towerPhi);

    energyDeposit.energy = (energyDeposit.energy <= 0) ? 0 : energyDeposit.energy;

    ret.push_back(energyDeposit);
  }

  return ret;
}

void StjTowerEnergyCorrectionForTracks::countTracksOnBemcTower(const StjTrack& track)
{
  if(track.exitDetectorId == 9 && track.exitTowerId != 0 && track.exitTowerId <= mNOfBemcTowers)
    mNtracksOnTower[track.exitTowerId]++;
}

double StjTowerEnergyCorrectionForTracks::correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId, float eta, float phi)
{
  if(bemcTowerId > mNOfBemcTowers) return energy;

  float theta=2.*atan(exp(-eta));

  //do a quick correction for hadronic MIP eneryg deposition:
  double MipE = 0.261*(1.+0.056*eta*eta)/sin(theta); //GeV

  return energy - mNtracksOnTower[bemcTowerId]*MipE;
}
