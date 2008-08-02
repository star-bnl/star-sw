// -*- mode: c++;-*-
// $Id: StjTowerEnergyCorrectionForTracks.h,v 1.2 2008/08/02 19:22:50 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

namespace StSpinJet {

class StjTowerEnergyCorrectionForTracks {

public:
  StjTowerEnergyCorrectionForTracks() { }
  virtual ~StjTowerEnergyCorrectionForTracks() { }

  StjTowerEnergyList operator()(const StjTowerEnergyList &energyDepositList, const StjTrackList& trackList);

  StjTowerEnergyList Do(const StjTowerEnergyList &energyDepositList, const StjTrackList& trackList);


private:

  void countTracksOnBemcTower(const StjTrack& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId, float eta, float phi);

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
