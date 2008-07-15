// -*- mode: c++;-*-
// $Id: TrackTowerEnergyListToFourList.h,v 1.1 2008/07/15 08:39:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TRACKTOWERENERGYLISTTOFOURLIST_H
#define TRACKTOWERENERGYLISTTOFOURLIST_H

#include "TrackList.h"
#include "TowerEnergyList.h"

#include <TClonesArray.h>

#include <utility>

namespace StSpinJet {

class TrackTowerEnergyListToFourList {

public:

  TClonesArray operator()(const std::pair<TrackList, TowerEnergyList>& inList);


};

}

#endif // TRACKTOWERENERGYLISTTOFOURLIST_H
