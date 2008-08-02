// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.3 2008/08/02 22:43:19 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUT_H
#define STJTOWERENERGYCUT_H

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

#endif // STJTOWERENERGYCUT_H
