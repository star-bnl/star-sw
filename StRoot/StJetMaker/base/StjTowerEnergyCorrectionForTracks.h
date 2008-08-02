// -*- mode: c++;-*-
// $Id: StjTowerEnergyCorrectionForTracks.h,v 1.3 2008/08/02 22:43:19 tai Exp $
#ifndef STJTOWERENERGYCORRECTIONFORTRACKS_H
#define STJTOWERENERGYCORRECTIONFORTRACKS_H

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

#endif // STJTOWERENERGYCORRECTIONFORTRACKS_H
