// -*- mode: c++;-*-
// $Id: TowerEnergyCut.h,v 1.2 2008/07/24 20:57:10 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUT_H
#define TOWERENERGYCUT_H

#include "TowerEnergyList.h"

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
