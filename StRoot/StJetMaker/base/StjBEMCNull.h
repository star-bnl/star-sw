// -*- mode: c++;-*-
// $Id: StjBEMCNull.h,v 1.2 2008/08/04 00:55:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJBEMCNULL_H
#define STJBEMCNULL_H

#include "StjBEMC.h"

class StjBEMCNull : public StjBEMC {

public:
  StjBEMCNull() { }
  virtual ~StjBEMCNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }

  ClassDef(StjBEMCNull, 1)

};

#endif // STJBEMCNULL_H
