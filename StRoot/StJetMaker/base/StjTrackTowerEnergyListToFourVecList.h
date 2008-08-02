// -*- mode: c++;-*-
// $Id: StjTrackTowerEnergyListToFourVecList.h,v 1.2 2008/08/02 19:22:56 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURVECLIST_H
#define TRACKTOWERENERGYLISTTOFOURVECLIST_H

#include "StjTrackList.h"
#include "StjTowerEnergyList.h"
#include "StjFourVecList.h"

#include <utility>

namespace StSpinJet {

class StjTrackTowerEnergyListToFourVecList {

public:

  StjFourVecList operator()(const std::pair<StjTrackList, StjTowerEnergyList>& inList);
  StjFourVecList operator()(const StjTrackList& trackList, const StjTowerEnergyList& energyList);

};

}

#endif // TRACKTOWERENERGYLISTTOFOURVECLIST_H
