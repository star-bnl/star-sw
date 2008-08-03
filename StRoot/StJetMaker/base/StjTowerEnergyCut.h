// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.4 2008/08/03 00:26:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUT_H
#define STJTOWERENERGYCUT_H

#include "StjTowerEnergyList.h"

class StjTowerEnergyCut {

public:
  StjTowerEnergyCut() { }
  virtual ~StjTowerEnergyCut() { }

  virtual bool operator()(const StjTowerEnergy& deposit) = 0;

private:

};

#endif // STJTOWERENERGYCUT_H
