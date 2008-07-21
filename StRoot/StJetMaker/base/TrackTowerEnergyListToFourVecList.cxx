// $Id: TrackTowerEnergyListToFourVecList.cxx,v 1.1 2008/07/21 17:25:04 tai Exp $
#include "TrackTowerEnergyListToFourVecList.h"

#include "TrackToFourVec.h"
#include "TowerEnergyToFourVec.h"

namespace StSpinJet {

FourVecList TrackTowerEnergyListToFourVecList::operator()(const std::pair<TrackList, TowerEnergyList>& inList)
{
  return operator()(inList.first, inList.second);
}

FourVecList TrackTowerEnergyListToFourVecList::operator()(const TrackList& trackList, const TowerEnergyList& energyList)
{
  FourVecList ret;

  TrackToFourVec track2four;
  TowerEnergyToFourVec tower2four;

  int fourvecId(1);
  for(TrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    FourVec four = track2four(*track);
    four.fourvecId = fourvecId++;
    ret.push_back(four);
  }

  for(TowerEnergyList::const_iterator tower = energyList.begin(); tower != energyList.end(); ++tower) {
    FourVec four = tower2four(*tower);
    four.fourvecId = fourvecId++;
    ret.push_back(four);
  }

  return ret;
}


}
