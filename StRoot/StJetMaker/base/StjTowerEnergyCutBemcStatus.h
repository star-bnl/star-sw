// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcStatus.h,v 1.4 2008/08/03 00:26:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTBEMCSTATUS_H
#define STJTOWERENERGYCUTBEMCSTATUS_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutBemcStatus : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutBemcStatus(int goodStatus = 1)
    : _goodStatus(goodStatus) { }
  virtual ~StjTowerEnergyCutBemcStatus() { }

  bool operator()(const StjTowerEnergy& tower)
  {
    if(tower.status != _goodStatus) return true;

    return false;
  }

private:

  int _goodStatus;
};

#endif // STJTOWERENERGYCUTBEMCSTATUS_H
