// -*- mode: c++;-*-
// $Id: StjTrackTowerEnergyListToFourVecList.h,v 1.3 2008/08/02 22:43:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOWERENERGYLISTTOFOURVECLIST_H
#define STJTRACKTOWERENERGYLISTTOFOURVECLIST_H

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

#endif // STJTRACKTOWERENERGYLISTTOFOURVECLIST_H
