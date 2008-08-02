// -*- mode: c++;-*-
// $Id: StjTowerEnergyCorrectionForTracks.h,v 1.1 2008/08/02 04:15:49 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

namespace StSpinJet {

class CorrectTowerEnergyForTracks {

public:
  CorrectTowerEnergyForTracks() { }
  virtual ~CorrectTowerEnergyForTracks() { }

  TowerEnergyList operator()(const TowerEnergyList &energyDepositList, const TrackList& trackList);

  TowerEnergyList Do(const TowerEnergyList &energyDepositList, const TrackList& trackList);


private:

  void countTracksOnBemcTower(const Track& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId, float eta, float phi);

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
