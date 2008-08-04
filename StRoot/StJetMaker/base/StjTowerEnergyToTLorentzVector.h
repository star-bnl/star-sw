// -*- mode: c++;-*-
// $Id: StjTowerEnergyToTLorentzVector.h,v 1.5 2008/08/04 06:10:26 tai Exp $
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
