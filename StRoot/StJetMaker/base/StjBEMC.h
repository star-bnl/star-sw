// -*- mode: c++;-*-
// $Id: StjBEMC.h,v 1.4 2008/08/03 00:26:26 tai Exp $
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


class StjBEMCNull : public StjBEMC {

public:
  StjBEMCNull() { }
  virtual ~StjBEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }
};

#endif // STJBEMC_H
