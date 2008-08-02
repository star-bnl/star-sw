// -*- mode: c++;-*-
// $Id: StjBEMC.h,v 1.2 2008/08/02 19:22:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMC_H
#define STJETBEMC_H

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

#endif // STJETBEMC_H
