// -*- mode: c++;-*-
// $Id: StjBEMC.h,v 1.5 2008/08/03 22:04:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMC_H
#define STJBEMC_H

#include "StjTowerEnergyList.h"

class StjBEMC {

public:
  StjBEMC() { }
  virtual ~StjBEMC() { }

  virtual void Init() { }

  virtual StjTowerEnergyList getEnergyList() = 0;

};

#endif // STJBEMC_H
