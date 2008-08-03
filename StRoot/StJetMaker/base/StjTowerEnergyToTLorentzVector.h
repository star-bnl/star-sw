// -*- mode: c++;-*-
// $Id: StjTowerEnergyToTLorentzVector.h,v 1.4 2008/08/03 00:26:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYTOTLORENTZVECTOR_H
#define STJTOWERENERGYTOTLORENTZVECTOR_H

#include <TLorentzVector.h>

class StjTowerEnergy;

class StjTowerEnergyToTLorentzVector {

public:
  StjTowerEnergyToTLorentzVector(double mass = 0 /* photon mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const StjTowerEnergy& deposit);

private:
  double _mass;
};

#endif // STJTOWERENERGYTOTLORENTZVECTOR_H
