// -*- mode: c++;-*-
// $Id: TowerEnergyCutBemcStatus.h,v 1.1 2008/07/13 09:38:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTBEMCSTATUS_H
#define TOWERENERGYCUTBEMCSTATUS_H

#include "TowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class TowerEnergyCutBemcStatus : public TowerEnergyCut {

public:
  TowerEnergyCutBemcStatus(int goodStatus = 1)
    : _goodStatus(goodStatus) { }
  virtual ~TowerEnergyCutBemcStatus() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.status != _goodStatus) return true;

    return false;
  }

private:

  int _goodStatus;
};

}

#endif // TOWERENERGYCUTBEMCSTATUS_H
