// -*- mode: c++;-*-
// $Id: StjTowerEnergyCutAdc.h,v 1.1 2008/11/27 07:35:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUTADC_H
#define STJTOWERENERGYCUTADC_H

#include "StjTowerEnergyCut.h"

class StjTowerEnergyCutAdc : public StjTowerEnergyCut {

public:
  StjTowerEnergyCutAdc(int min = 0, double factor = 2.0)
    : _min(min), _factor(factor) { }
  virtual ~StjTowerEnergyCutAdc() { }

  bool operator()(const StjTowerEnergy& tower)
  {
    if(tower.adc - tower.pedestal <= _min) return true;

    if(tower.adc - tower.pedestal <= _factor*tower.rms) return true;

    return false;
  }

private:

  int _min;
  double _factor;

  ClassDef(StjTowerEnergyCutAdc, 1)

};

#endif // STJTOWERENERGYCUTADC_H
