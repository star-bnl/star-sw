// -*- mode: c++;-*-
// $Id: StjBEMC.h,v 1.3 2008/08/02 22:43:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMC_H
#define STJBEMC_H

#include "StjTowerEnergyList.h"

namespace StSpinJet {

class StjBEMC {

public:
  StjBEMC() { }
  virtual ~StjBEMC() { }

  virtual void Init() { }

  virtual StjTowerEnergyList getEnergyList() = 0;
};


class StjBEMCNull : public StjBEMC {

public:
  StjBEMCNull() { }
  virtual ~StjBEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }
};

}

#endif // STJBEMC_H
