// -*- mode: c++;-*-
// $Id: StjBEMCNull.h,v 1.1 2008/11/27 07:35:22 tai Exp $
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
