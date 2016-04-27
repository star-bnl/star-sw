// -*- mode: C++ -*-

//
// Grant Webb <gdwebb@bnl.gov>
// Brookhaven National Laboratory
// 3 August 2015
//

#ifndef STJ_TOWER_REGION_H
#define STJ_TOWER_REGION_H

// STAR
#include "StjTowerEnergyList.h"
#include <iostream>
using namespace std;
//Local
#include "StjAbstractTowerRegion.h"
//#include "StJetFinder/StJetFinder.h"
//#include "StSpinPool/StJetEvent/StJetEventTypes.h" // Need to implement the jet candidate somehow

class StjTowerRegion : public StjAbstractTowerRegion {
public: 
  StjTowerRegion(float phiplus, float phiminus, float deta) : mphiplus(phiplus),  mphiminus(phiminus), mdeta(deta){}
  virtual ~StjTowerRegion() {}
 
  StjTowerEnergyList Do(const StjTowerEnergyList &towerList, const StJetCandidate* leadingjet, const TString bname);
  float phiplus() const {return mphiplus;}
  float phiminus() const {return mphiminus;}
  float deta() const { return mdeta;}
  void setphiplus(float phiplus) { mphiplus = phiplus;} 
  void setphiminus(float phiminus) { mphiminus = phiminus;} 
  void setdeta(float deta) {mdeta = deta;}
private: 
  float mphiplus;
  float mphiminus;
  float mdeta;
  ClassDef(StjTowerRegion,0);
};

#endif // STJ_TOWER_REGION_H 
