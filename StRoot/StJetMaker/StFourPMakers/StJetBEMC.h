// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.6 2008/07/13 10:02:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETBEMC_H
#define STJETBEMC_H

#include "TowerEnergyList.h"

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
