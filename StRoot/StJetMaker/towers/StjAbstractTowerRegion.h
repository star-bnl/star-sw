// -*- mode: C++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Lab
// 8 August 2015
//

#ifndef STJ_ABSTRACT_TOWER_REGION_H
#define STJ_ABSTRACT_TOWER_REGION_H

// ROOT
#include "TObject.h"

// STAR
#include "StjTowerEnergyList.h"
#include "StSpinPool/StJetEvent/StJetCandidate.h"

class StjAbstractTowerRegion : public TObject {
public:
  StjAbstractTowerRegion() {}
  virtual ~StjAbstractTowerRegion() {}

  StjTowerEnergyList operator()(const StjTowerEnergyList& towerList, const StJetCandidate* jet, const TString name)
  {
    return Do(towerList, jet, name);
  }

  virtual StjTowerEnergyList Do(const StjTowerEnergyList& towerList, const StJetCandidate* jet, const TString name) = 0;

  ClassDef(StjAbstractTowerRegion,0);
};

#endif // STJ_ABSTRACT_TOWER_REGION_H

