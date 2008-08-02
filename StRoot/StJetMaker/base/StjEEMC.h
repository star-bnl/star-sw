// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.1 2008/08/02 04:15:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETEEMC_H
#define STJETEEMC_H

#include "StjTowerEnergyList.h"

namespace StSpinJet {

class StJetEEMC {

public:
  StJetEEMC() { }
  virtual ~StJetEEMC() { }

  virtual void Init() { }

  virtual TowerEnergyList getEnergyList() = 0;
};


class StJetEEMCNull : public StJetEEMC {

public:
  StJetEEMCNull() { }
  virtual ~StJetEEMCNull() { }

  TowerEnergyList getEnergyList() { return TowerEnergyList(); }
};


}

#endif // STJETEEMC_H
