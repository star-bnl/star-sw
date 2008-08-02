// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcStatus.h,v 1.2 2008/08/02 19:22:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTBEMCSTATUS_H
#define TOWERENERGYCUTBEMCSTATUS_H

#include "StjTowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class StjTowerEnergyCutBemcStatus : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutBemcStatus(int goodStatus = 1)
    : _goodStatus(goodStatus) { }
  virtual ~StjTowerEnergyCutBemcStatus() { }

  bool operator()(const StSpinJet::StjTowerEnergy& tower)
  {
    if(tower.status != _goodStatus) return true;

    return false;
  }

private:

  int _goodStatus;
};

}

#endif // TOWERENERGYCUTBEMCSTATUS_H
