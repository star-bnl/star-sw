// -*- mode: c++;-*-
// $Id: StjBEMCNull.h,v 1.1 2008/08/03 22:04:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMCNULL_H
#define STJBEMCNULL_H

#include "StjBEMC.h"

class StjBEMCNull : public StjBEMC {

public:
  StjBEMCNull() { }
  virtual ~StjBEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }

};

#endif // STJBEMCNULL_H
