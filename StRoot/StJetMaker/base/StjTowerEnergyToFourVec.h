// -*- mode: c++;-*-
// $Id: StjTowerEnergyToFourVec.h,v 1.3 2008/08/02 22:43:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYTOFOURVEC_H
#define STJTOWERENERGYTOFOURVEC_H

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

#endif // STJTOWERENERGYTOFOURVEC_H
