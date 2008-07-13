// -*- mode: c++;-*-
// $Id: TowerEnergyCutBemcWestOnly.h,v 1.1 2008/07/13 09:38:01 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTBEMCWESTONLY_H
#define TOWERENERGYCUTBEMCWESTONLY_H

#include "TowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class TowerEnergyCutBemcWestOnly : public TowerEnergyCut {

public:
  TowerEnergyCutBemcWestOnly() { }
  virtual ~TowerEnergyCutBemcWestOnly() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.detectorId != 9) return true;

    if(tower.towerId > 2400) return true;

    return false;
  }

private:
  
};

}

#endif // TOWERENERGYCUT2003BEMCTOWER_H
