// -*- mode: c++;-*-
// $Id: TrackTowerEnergyListToFourVecList.h,v 1.1 2008/07/21 17:25:05 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURVECLIST_H
#define TRACKTOWERENERGYLISTTOFOURVECLIST_H

#include "TrackList.h"
#include "TowerEnergyList.h"
#include "FourVecList.h"

#include <utility>

namespace StSpinJet {

class TrackTowerEnergyListToFourVecList {

public:

  FourVecList operator()(const std::pair<TrackList, TowerEnergyList>& inList);
  FourVecList operator()(const TrackList& trackList, const TowerEnergyList& energyList);

};

}

#endif // TRACKTOWERENERGYLISTTOFOURVECLIST_H
