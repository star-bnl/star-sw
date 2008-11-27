// -*- mode: c++;-*-
// $Id: StjEEMCNull.h,v 1.1 2008/11/27 07:35:24 tai Exp $
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
