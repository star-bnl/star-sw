// -*- mode: c++;-*-
// $Id: StjBEMCSockeVariation.h,v 1.1 2009/09/03 23:10:39 pibero Exp $
#ifndef STJBEMCSOCKEVARIATION_H
#define STJBEMCSOCKEVARIATION_H

#include "StjBEMC.h"

class StjTowerEnergyListVariation;

class StjBEMCSockeVariation : public StjBEMC {

public:
  StjBEMCSockeVariation(StjBEMC* src, StjTowerEnergyListVariation* variation)
    : _src(src), _variation(variation) { }
  virtual ~StjBEMCSockeVariation() { }

  StjTowerEnergyList getEnergyList();

private:

  StjBEMC* _src;
  StjTowerEnergyListVariation* _variation;

  ClassDef(StjBEMCSockeVariation, 1)

};

#endif // STJBEMCSOCKEVARIATION_H
