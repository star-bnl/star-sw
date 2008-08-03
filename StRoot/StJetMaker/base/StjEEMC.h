// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.5 2008/08/03 22:04:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMC_H
#define STJEEMC_H

#include "StjTowerEnergyList.h"

class StjEEMC {

public:
  StjEEMC() { }
  virtual ~StjEEMC() { }

  virtual void Init() { }

  virtual StjTowerEnergyList getEnergyList() = 0;

};

#endif // STJEEMC_H
