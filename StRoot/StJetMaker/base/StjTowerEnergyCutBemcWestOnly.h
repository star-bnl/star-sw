// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutBemcWestOnly.h,v 1.3 2008/08/02 22:43:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTBEMCWESTONLY_H
#define STJTOWERENERGYCUTBEMCWESTONLY_H

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

#endif // STJTOWERENERGYCUTBEMCWESTONLY_H
