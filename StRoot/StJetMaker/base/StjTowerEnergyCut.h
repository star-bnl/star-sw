// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.8 2008/11/04 08:07:01 tai Exp $
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
