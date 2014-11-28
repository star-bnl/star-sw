// -*- mode: c++;-*-
// $Id: StjTowerEnergyToTLorentzVector.h,v 1.1 2008/11/27 07:35:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYTOTLORENTZVECTOR_H
#define STJTOWERENERGYTOTLORENTZVECTOR_H

#include <TObject.h>

#include <TLorentzVector.h>

class StjTowerEnergy;

class StjTowerEnergyToTLorentzVector : public TObject {

public:
  StjTowerEnergyToTLorentzVector(double mass = 0 /* photon mass as default */)
    : _mass(mass) { }
  TLorentzVector operator()(const StjTowerEnergy& deposit);

private:
  double _mass;

  ClassDef(StjTowerEnergyToTLorentzVector, 1)

};

#endif // STJTOWERENERGYTOTLORENTZVECTOR_H
