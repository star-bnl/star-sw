// -*- mode: c++;-*-
// $Id: StjTowerEnergyToTLorentzVector.h,v 1.2 2008/08/02 19:22:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYTOTLORENTZVECTOR_H
#define TOWERENERGYTOTLORENTZVECTOR_H

#include <TLorentzVector.h>

namespace StSpinJet {

class StjTowerEnergy;

class StjTowerEnergyToTLorentzVector {

public:
  StjTowerEnergyToTLorentzVector(double mass = 0 /* photon mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const StjTowerEnergy& deposit);

private:
  double _mass;
};

}

#endif // TOWERENERGYTOTLORENTZVECTOR_H
