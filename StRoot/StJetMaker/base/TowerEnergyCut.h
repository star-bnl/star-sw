// -*- mode: c++;-*-
// $Id: TowerEnergyCut.h,v 1.1 2008/07/21 17:24:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUT_H
#define TOWERENERGYCUT_H

namespace StJetTowerEnergyCut {

class TowerEnergyCut {

public:
  TowerEnergyCut() { }
  virtual ~TowerEnergyCut() { }

  virtual bool operator()(const StSpinJet::TowerEnergy& deposit) = 0;

private:

};

}

#endif // TOWERENERGYCUT_H
