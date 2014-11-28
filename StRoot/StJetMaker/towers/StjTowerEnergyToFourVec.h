// -*- mode: c++;-*-
// $Id: StjTowerEnergyToFourVec.h,v 1.1 2008/11/27 07:35:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYTOFOURVEC_H
#define STJTOWERENERGYTOFOURVEC_H

#include <TObject.h>

#include "StjFourVecList.h"

#include "StjTowerEnergyToTLorentzVector.h"

class StjTowerEnergy;

class StjTowerEnergyToFourVec : public TObject {
public:
  StjTowerEnergyToFourVec(double mass = 0 /* photon mass as default */)
    : _towerenergy2tlorentzvector(*(new StjTowerEnergyToTLorentzVector(mass))) { }
  virtual ~StjTowerEnergyToFourVec() { delete &_towerenergy2tlorentzvector; }
  StjFourVec operator()(const StjTowerEnergy& towerEnergy);

private:
  StjTowerEnergyToTLorentzVector& _towerenergy2tlorentzvector;
  ClassDef(StjTowerEnergyToFourVec, 1)

};

#endif // STJTOWERENERGYTOFOURVEC_H
