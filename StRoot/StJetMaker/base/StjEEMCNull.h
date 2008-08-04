// -*- mode: c++;-*-
// $Id: StjEEMCNull.h,v 1.2 2008/08/04 00:55:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJEEMCNULL_H
#define STJEEMCNULL_H

#include "StjEEMC.h"

class StjEEMCNull : public StjEEMC {

public:
  StjEEMCNull() { }
  virtual ~StjEEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }

  ClassDef(StjEEMCNull, 1)

};

#endif // STJEEMCNULL_H
