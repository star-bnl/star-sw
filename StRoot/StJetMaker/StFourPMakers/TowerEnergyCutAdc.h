// -*- mode: c++;-*-
// $Id: TowerEnergyCutAdc.h,v 1.1 2008/07/13 09:38:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef TOWERENERGYCUTADC_H
#define TOWERENERGYCUTADC_H

#include "TowerEnergyCut.h"

namespace StJetTowerEnergyCut {

class TowerEnergyCutAdc : public TowerEnergyCut {

public:
  TowerEnergyCutAdc(int min = 0, double factor = 2.0)
    : _min(min), _factor(factor) { }
  virtual ~TowerEnergyCutAdc() { }

  bool operator()(const StSpinJet::TowerEnergy& tower)
  {
    if(tower.adc - tower.pedestal <= _min) return true;

    if(tower.adc - tower.pedestal <= _factor*tower.rms) return true;

    return false;
  }

private:

  int _min;
  double _factor;
};

}

#endif // TOWERENERGYCUTADC_H
