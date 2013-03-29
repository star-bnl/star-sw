// -*- mode: C++ -*-

//
// Grant Webb <grant.webb@uky.edu>
// University of Kentucky
// 6 March 2012
//

#ifndef STJ_TOWER_ENERGY_FRACTION_H
#define STJ_TOWER_ENERGY_FRACTION_H

// STAR
#include "StjTowerEnergyList.h"

//Local
#include "StjAbstractTower.h"

class StjTowerEnergyFraction : public StjAbstractTower {
public:
  StjTowerEnergyFraction(float fraction) : mFraction(fraction) {}
  virtual ~StjTowerEnergyFraction() {}
  
  StjTowerEnergyList Do(const StjTowerEnergyList& energyDepositList);
  
  float fraction() const { return mFraction; }
  void setFraction(float fraction) { mFraction = fraction; }

private:
  float mFraction;

  ClassDef(StjTowerEnergyFraction,0);

};

#endif // STJ_TOWER_ENERGYç_FRACTION_H
