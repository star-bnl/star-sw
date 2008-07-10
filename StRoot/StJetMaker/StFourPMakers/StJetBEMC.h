// -*- mode: c++;-*-
// $Id: StJetBEMC.h,v 1.5 2008/07/10 20:15:20 tai Exp $
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
