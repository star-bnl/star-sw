// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.4 2008/08/03 00:26:27 tai Exp $
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


class StjEEMCNull : public StjEEMC {

public:
  StjEEMCNull() { }
  virtual ~StjEEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }
};

#endif // STJEEMC_H
