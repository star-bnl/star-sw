// $Id: StjTrackTowerEnergyListToFourVecList.cxx,v 1.1 2008/11/27 07:09:38 tai Exp $
#include "StjTrackTowerEnergyListToFourVecList.h"

#include "StjTrackToFourVec.h"
#include "StjTowerEnergyToFourVec.h"

ClassImp(StjTrackTowerEnergyListToFourVecList)

StjFourVecList StjTrackTowerEnergyListToFourVecList::operator()(const std::pair<StjTrackList, StjTowerEnergyList>& inList)
{
  return operator()(inList.first, inList.second);
}

StjFourVecList StjTrackTowerEnergyListToFourVecList::operator()(const StjTrackList& trackList, const StjTowerEnergyList& energyList)
{
  StjFourVecList ret;

  StjTrackToFourVec track2four;
  StjTowerEnergyToFourVec tower2four;

  int fourvecId(1);
  for(StjTrackList::const_iterator track = trackList.begin(); track != trackList.end(); ++track) {
    StjFourVec four = track2four(*track);
    four.fourvecId = fourvecId++;
    ret.push_back(four);
  }

  for(StjTowerEnergyList::const_iterator tower = energyList.begin(); tower != energyList.end(); ++tower) {
    StjFourVec four = tower2four(*tower);
    four.fourvecId = fourvecId++;
    ret.push_back(four);
  }

  return ret;
}
