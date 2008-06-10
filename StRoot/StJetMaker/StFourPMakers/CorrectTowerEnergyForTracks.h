// -*- mode: c++;-*-
// $Id: CorrectTowerEnergyForTracks.h,v 1.1 2008/06/10 09:17:57 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "TowerEnergyDeposit.h"

#include <vector>
#include <utility>


class StMuTrack;
class StMuDstMaker;

namespace StSpinJet {

class CorrectTowerEnergyForTracks {

public:
  CorrectTowerEnergyForTracks(StMuDstMaker* uDstMaker);
  virtual ~CorrectTowerEnergyForTracks() { }

  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;

  TowerEnergyDepositList Do(const TowerEnergyDepositList &energyDepositList, const TrackList& trackList);

private:

  void countTracksOnBemcTower(const StMuTrack& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  StMuDstMaker* mMuDstMaker;

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
