// -*- mode: c++;-*-
// $Id: StjBEMC.h,v 1.1 2008/08/02 04:15:09 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMC_H
#define STJETBEMC_H

#include "StjTowerEnergyList.h"

namespace StSpinJet {

class StJetBEMC {

public:
  StJetBEMC() { }
  virtual ~StJetBEMC() { }

  virtual void Init() { }

  virtual TowerEnergyList getEnergyList() = 0;
};


class StJetBEMCNull : public StJetBEMC {

public:
  StJetBEMCNull() { }
  virtual ~StJetBEMCNull() { }

  TowerEnergyList getEnergyList() { return TowerEnergyList(); }
};

}

#endif // STJETBEMC_H
