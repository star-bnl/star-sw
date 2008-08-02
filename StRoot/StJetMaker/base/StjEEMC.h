// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.3 2008/08/02 22:43:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMC_H
#define STJEEMC_H

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

#endif // STJEEMC_H
