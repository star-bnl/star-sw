// -*- mode: c++;-*-
// $Id: CorrectTowerEnergyForTracks.h,v 1.2 2008/07/07 22:12:29 tai Exp $
#ifndef CORRECTTOWERENERGYFORTRACKS_H
#define CORRECTTOWERENERGYFORTRACKS_H

#include "TowerEnergyDeposit.h"

#include <vector>
#include <utility>


class StMuTrack;
class StMuDstMaker;

namespace StSpinJet {

class StMuTrackEmu;

class CorrectTowerEnergyForTracks {

public:
  CorrectTowerEnergyForTracks(StMuDstMaker* uDstMaker);
  virtual ~CorrectTowerEnergyForTracks() { }

  //  typedef std::vector<std::pair<const StMuTrack*, int> > TrackList;
  typedef std::vector<StSpinJet::StMuTrackEmu*> TrackList;

  TowerEnergyDepositList Do(const TowerEnergyDepositList &energyDepositList, const TrackList& trackList);

private:

  void countTracksOnBemcTower(const StMuTrackEmu& track);

  double correctBemcTowerEnergyForTracks_(double energy, int bemcTowerId);

  StMuDstMaker* mMuDstMaker;

  static const int mNOfBemcTowers = 4800;

  int mNtracksOnTower[mNOfBemcTowers + 1]; // indexed form [1,4800] (number of tracks incident on this tower)

};

}

#endif // CORRECTTOWERENERGYFORTRACKS_H
