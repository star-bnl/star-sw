// -*- mode: c++;-*-
// $Id: TrackTowerEnergyListToFourList.h,v 1.3 2008/07/17 02:39:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURLIST_H
#define TRACKTOWERENERGYLISTTOFOURLIST_H

#include "TrackList.h"
#include "TowerEnergyList.h"

#include <TObjArray.h>

#include <utility>

namespace StSpinJet {

class TrackTowerEnergyListToFourList {

public:

  TObjArray operator()(const std::pair<TrackList, TowerEnergyList>& inList);
  TObjArray operator()(const TrackList& trackList, const TowerEnergyList& energyList);


};

}

#endif // TRACKTOWERENERGYLISTTOFOURLIST_H
