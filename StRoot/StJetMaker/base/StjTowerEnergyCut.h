// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.2 2008/08/02 19:22:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUT_H
#define TOWERENERGYCUT_H

#include "StjTowerEnergyList.h"

namespace StJetTowerEnergyCut {

class StjTowerEnergyCut {

public:
  StjTowerEnergyCut() { }
  virtual ~StjTowerEnergyCut() { }

  virtual bool operator()(const StSpinJet::StjTowerEnergy& deposit) = 0;

private:

};

}

#endif // TOWERENERGYCUT_H
