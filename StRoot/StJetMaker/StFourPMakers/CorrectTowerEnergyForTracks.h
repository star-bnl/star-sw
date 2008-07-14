// -*- mode: c++;-*-
// $Id: CorrectTowerEnergyForTracks.h,v 1.6 2008/07/14 23:07:30 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "TowerEnergyList.h"
#include "TrackList.h"

#include <vector>
#include <utility>

namespace StSpinJet {

class StMuTrackEmu;

class CorrectTowerEnergyForTracks {

public:
  CorrectTowerEnergyForTracks();
  virtual ~CorrectTowerEnergyForTracks() { }

  TowerEnergyList Do(const TowerEnergyList &energyDepositList, const TrackList& trackList);

private:

  void countTracksOnBemcTower(const Track& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId, float eta, float phi);

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
