// -*- mode: c++;-*-
// $Id: StjTowerEnergyListVariation.h,v 1.1 2008/08/04 20:47:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYVARIATION_H
#define STJTOWERENERGYVARIATION_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjTowerEnergyListVariation : public TObject {

public:
  StjTowerEnergyListVariation(double ratio = 0.1)
    : _ratio(ratio) { }
  virtual ~StjTowerEnergyListVariation() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

private:

  StjTowerEnergy vary(const StjTowerEnergy& deposit);

  double _ratio;

  ClassDef(StjTowerEnergyListVariation, 1)

};

#endif // STJTOWERENERGYVARIATION_H
