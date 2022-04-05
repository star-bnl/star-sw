// -*- mode: C++ -*-

//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky
// 8 March 2013
//

#ifndef STJ_ABSTRACT_TOWER_H
#define STJ_ABSTRACT_TOWER_H

// ROOT
#include "TObject.h"

// STAR
#include "StjTowerEnergyList.h"

class StjAbstractTower : public TObject {
public:
  StjAbstractTower() {}
  virtual ~StjAbstractTower() {}

  StjTowerEnergyList operator()(const StjTowerEnergyList& towerList)
  {
    return Do(towerList);
  }

  virtual StjTowerEnergyList Do(const StjTowerEnergyList& towerList) = 0;

  ClassDef(StjAbstractTower,0);
};

#endif // STJ_ABTRACT_TOWERS_H

