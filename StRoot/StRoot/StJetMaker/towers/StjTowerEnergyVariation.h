// -*- mode: c++;-*-
// $Id: StjTowerEnergyVariation.h,v 1.1 2008/11/27 07:35:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATION_H
#define STJTOWERENERGYVARIATION_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjTowerEnergyVariation : public TObject {

public:
  StjTowerEnergyVariation() { }
  virtual ~StjTowerEnergyVariation() { }

  virtual StjTowerEnergy operator()(const StjTowerEnergy& deposit) = 0;

private:

  ClassDef(StjTowerEnergyVariation, 1)

};

#endif // STJTOWERENERGYVARIATION_H
