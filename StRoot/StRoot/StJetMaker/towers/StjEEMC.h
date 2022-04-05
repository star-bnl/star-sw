// -*- mode: c++;-*-
// $Id: StjEEMC.h,v 1.1 2008/11/27 07:35:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMC_H
#define STJEEMC_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjEEMC : public TObject {

public:
  StjEEMC() { }
  virtual ~StjEEMC() { }

  virtual void Init() { }

  virtual StjTowerEnergyList getEnergyList() = 0;

  ClassDef(StjEEMC, 1)

};

#endif // STJEEMC_H
