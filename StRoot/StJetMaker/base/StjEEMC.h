// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.2 2008/08/02 19:22:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETEEMC_H
#define STJETEEMC_H

#include "StjTowerEnergyList.h"

namespace StSpinJet {

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


}

#endif // STJETEEMC_H
