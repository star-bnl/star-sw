// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcStatus.h,v 1.1 2008/11/27 07:35:28 tai Exp $
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
  ClassDef(StjTowerEnergyCutBemcStatus, 1)

};

#endif // STJTOWERENERGYCUTBEMCSTATUS_H
