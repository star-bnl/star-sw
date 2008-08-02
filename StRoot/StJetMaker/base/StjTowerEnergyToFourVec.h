// -*- mode: c++;-*-
// $Id: StjTowerEnergyToFourVec.h,v 1.2 2008/08/02 19:22:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYTOFOURVEC_H
#define TOWERENERGYTOFOURVEC_H

#include "StjFourVecList.h"

#include "StjTowerEnergyToTLorentzVector.h"

namespace StSpinJet {

class StjTowerEnergy;

class StjTowerEnergyToFourVec {
public:
  StjTowerEnergyToFourVec(double mass = 0 /* photon mass as default */)
    : _towerenergy2tlorentzvector(*(new StjTowerEnergyToTLorentzVector(mass))) { }
  virtual ~StjTowerEnergyToFourVec() { delete &_towerenergy2tlorentzvector; }
  StjFourVec operator()(const StjTowerEnergy& towerEnergy);

private:
  StjTowerEnergyToTLorentzVector& _towerenergy2tlorentzvector;
};

}

#endif // TOWERENERGYTOFOURVEC_H
