// -*- mode: c++;-*-
// $Id: StjTowerEnergyVariation.h,v 1.5 2008/08/04 06:10:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATION_H
#define STJTOWERENERGYVARIATION_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjTowerEnergyVariation : public TObject {

public:
  StjTowerEnergyVariation(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StjTowerEnergyVariation() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

private:

  StjTowerEnergy vary(const StjTowerEnergy& deposit);

  double _ratio;

  ClassDef(StjTowerEnergyVariation, 1)

};

#endif // STJTOWERENERGYVARIATION_H
