// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcWestOnly.h,v 1.4 2008/08/03 00:26:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTBEMCWESTONLY_H
#define STJTOWERENERGYCUTBEMCWESTONLY_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutBemcWestOnly : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutBemcWestOnly() { }
  virtual ~StjTowerEnergyCutBemcWestOnly() { }

  bool operator()(const StjTowerEnergy& tower)
  {
    if(tower.detectorId != 9) return true;

    if(tower.towerId > 2400) return true;

    return false;
  }

private:
  
};

#endif // STJTOWERENERGYCUTBEMCWESTONLY_H
