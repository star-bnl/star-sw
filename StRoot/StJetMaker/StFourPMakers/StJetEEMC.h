// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.6 2008/07/13 10:02:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETEEMC_H
#define STJETEEMC_H

#include "TowerEnergyList.h"

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
