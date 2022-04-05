// -*- mode: c++;-*-
// $Id: StjTowerEnergyCorrectionForTracks.h,v 1.1 2008/11/27 07:35:25 tai Exp $
#ifndef STJTOWERENERGYCORRECTIONFORTRACKS_H
#define STJTOWERENERGYCORRECTIONFORTRACKS_H

#include <TObject.h>

#include "StjTowerEnergyList.h"
#include "StjTrackList.h"

class StjTowerEnergyCorrectionForTracks : public TObject {

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

  ClassDef(StjTowerEnergyCorrectionForTracks, 1)

};

#endif // STJTOWERENERGYCORRECTIONFORTRACKS_H
