// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutTowerId.h,v 1.1 2008/11/27 07:35:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTTOWERID_H
#define STJTOWERENERGYCUTTOWERID_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutTowerId : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutTowerId(int towerId = 0)
    :_towerId(towerId) { }
  virtual ~StjTowerEnergyCutTowerId() { }

  bool operator()(const StjTowerEnergy& tower)
  {
    if(tower.detectorId != 9) return true;

    if(tower.towerId == _towerId) return true;

    return false;
  }

private:

  int _towerId;
  
  ClassDef(StjTowerEnergyCutTowerId, 1)

};

#endif // STJTOWERENERGYCUTTOWERID_H
