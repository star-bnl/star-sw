// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcWestOnly.h,v 1.2 2008/08/02 19:22:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTBEMCWESTONLY_H
#define TOWERENERGYCUTBEMCWESTONLY_H

#include "StjTowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class StjTowerEnergyCutBemcWestOnly : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutBemcWestOnly() { }
  virtual ~StjTowerEnergyCutBemcWestOnly() { }

  bool operator()(const StSpinJet::StjTowerEnergy& tower)
  {
    if(tower.detectorId != 9) return true;

    if(tower.towerId > 2400) return true;

    return false;
  }

private:
  
};

}

#endif // TOWERENERGYCUT2003BEMCTOWER_H
