// -*- mode: c++;-*-
// $Id: StjEEMCNull.h,v 1.1 2008/08/03 22:04:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMCNULL_H
#define STJEEMCNULL_H

#include "StjEEMC.h"

class StjEEMCNull : public StjEEMC {

public:
  StjEEMCNull() { }
  virtual ~StjEEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }

};

#endif // STJEEMCNULL_H
