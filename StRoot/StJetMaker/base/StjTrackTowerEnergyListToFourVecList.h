// -*- mode: c++;-*-
// $Id: StjTrackTowerEnergyListToFourVecList.h,v 1.1 2008/08/02 04:16:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURVECLIST_H
#define TRACKTOWERENERGYLISTTOFOURVECLIST_H

#include "StjTrackList.h"
#include "StjTowerEnergyList.h"
#include "StjFourVecList.h"

#include <utility>

namespace StSpinJet {

class TrackTowerEnergyListToFourVecList {

public:

  FourVecList operator()(const std::pair<TrackList, TowerEnergyList>& inList);
  FourVecList operator()(const TrackList& trackList, const TowerEnergyList& energyList);

};

}

#endif // TRACKTOWERENERGYLISTTOFOURVECLIST_H
