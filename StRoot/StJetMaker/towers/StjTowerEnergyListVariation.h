// -*- mode: c++;-*-
// $Id: StjTowerEnergyListVariation.h,v 1.1 2008/11/27 07:35:32 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYLISTVARIATION_H
#define STJTOWERENERGYLISTVARIATION_H

#include <TObject.h>

#include "StjTowerEnergyList.h"
#include "StjTowerEnergyVariation.h"

#include <vector>

class StjTowerEnergyListVariation : public TObject {

public:
  StjTowerEnergyListVariation() { }
  virtual ~StjTowerEnergyListVariation() { }
  
  StjTowerEnergyList operator()(const StjTowerEnergyList& energyList);

  void addVariation(StjTowerEnergyVariation* var) {
    _varList.push_back(var);
  }

  typedef std::vector<StjTowerEnergyVariation*> VarList;
  VarList getVariationList() { return _varList; }

private:

  StjTowerEnergy vary(const StjTowerEnergy& deposit);

  VarList _varList;

  ClassDef(StjTowerEnergyListVariation, 1)

};

#endif // STJTOWERENERGYLISTVARIATION_H
