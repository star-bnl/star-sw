// -*- mode: c++;-*-
// $Id: StJetEEMC.h,v 1.5 2008/07/10 20:15:21 tai Exp $
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
