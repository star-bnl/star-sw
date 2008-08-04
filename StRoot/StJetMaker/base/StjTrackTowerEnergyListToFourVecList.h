// -*- mode: c++;-*-
// $Id: StjTrackTowerEnergyListToFourVecList.h,v 1.5 2008/08/04 06:10:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKTOWERENERGYLISTTOFOURVECLIST_H
#define STJTRACKTOWERENERGYLISTTOFOURVECLIST_H

#include <TObject.h>

#include "StjTrackList.h"
#include "StjTowerEnergyList.h"
#include "StjFourVecList.h"

#include <utility>

class StjTrackTowerEnergyListToFourVecList : public TObject {

public:

  StjFourVecList operator()(const std::pair<StjTrackList, StjTowerEnergyList>& inList);
  StjFourVecList operator()(const StjTrackList& trackList, const StjTowerEnergyList& energyList);

  ClassDef(StjTrackTowerEnergyListToFourVecList, 1)

};

#endif // STJTRACKTOWERENERGYLISTTOFOURVECLIST_H
