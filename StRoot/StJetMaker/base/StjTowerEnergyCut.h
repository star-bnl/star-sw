// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.7 2008/11/04 07:52:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTOWERENERGYCUT_H
#define STJTOWERENERGYCUT_H
#include "StkCut.h"
#include "StjTowerEnergyList.h"
typedef StkCut<StjTowerEnergy> StjTowerEnergyCut;
#endif // STJTOWERENERGYCUT_H
// -*- mode: c++;-*-
// $Id: StjTowerEnergyCut.h,v 1.7 2008/11/04 07:52:50 tai Exp $
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
