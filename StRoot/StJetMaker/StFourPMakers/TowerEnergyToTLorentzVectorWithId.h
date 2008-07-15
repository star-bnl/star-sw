// -*- mode: c++;-*-
// $Id: TowerEnergyToTLorentzVectorWithId.h,v 1.1 2008/07/15 06:23:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYTOTLORENTZVECTORWITHID_H
#define TOWERENERGYTOTLORENTZVECTORWITHID_H

#include "TLorentzVectorWithId.h"

#include "TowerEnergyToTLorentzVector.h"

namespace StSpinJet {

class TowerEnergy;

class TowerEnergyToTLorentzVectorWithId {
public:
  TowerEnergyToTLorentzVectorWithId(double mass = 0 /* photon mass as default */)
    : _towerenergy2tlorentzvector(*(new TowerEnergyToTLorentzVector(mass))) { }
  TLorentzVectorWithId operator()(const TowerEnergy& deposit);

private:
  TowerEnergyToTLorentzVector& _towerenergy2tlorentzvector;
};

}

#endif // TOWERENERGYTOTLORENTZVECTORWITHID_H
