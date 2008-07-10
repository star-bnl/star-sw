// -*- mode: c++;-*-
// $Id: CorrectTowerEnergyForTracks.h,v 1.4 2008/07/10 20:15:19 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "TowerEnergyList.h"

#include <vector>
#include <utility>

namespace StSpinJet {

class StMuTrackEmu;

class CorrectTowerEnergyForTracks {

public:
  CorrectTowerEnergyForTracks();
  virtual ~CorrectTowerEnergyForTracks() { }

  typedef std::vector<StMuTrackEmu*> TrackList;

  TowerEnergyList Do(const TowerEnergyList &energyDepositList, const TrackList& trackList);

private:

  void countTracksOnBemcTower(const StMuTrackEmu& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
