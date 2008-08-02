// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.1 2008/08/02 04:15:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUT_H
#define TOWERENERGYCUT_H

#include "StjTowerEnergyList.h"

namespace StJetTowerEnergyCut {

class TowerEnergyCut {

public:
  TowerEnergyCut() { }
  virtual ~TowerEnergyCut() { }

  virtual bool operator()(const StSpinJet::TowerEnergy& deposit) = 0;

private:

};

}

#endif // TOWERENERGYCUT_H
