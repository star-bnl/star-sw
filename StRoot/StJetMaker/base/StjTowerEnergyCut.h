// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.5 2008/08/04 02:48:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUT_H
#define STJTOWERENERGYCUT_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjTowerEnergyCut : public TObject {

public:
  StjTowerEnergyCut() { }
  virtual ~StjTowerEnergyCut() { }

  virtual bool operator()(const StjTowerEnergy& deposit) = 0;

private:

  ClassDef(StjTowerEnergyCut, 1)

};

#endif // STJTOWERENERGYCUT_H
