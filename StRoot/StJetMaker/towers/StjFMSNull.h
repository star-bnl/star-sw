// -*- mode: c++;-*-
// $Id: StjFMSNull.h,v 1.1 2017/05/22 19:36:06 zchang Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFMSNULL_H
#define STJFMSNULL_H

#include "StjFMS.h"

class StjFMSNull : public StjFMS {

public:
  StjFMSNull() { }
  virtual ~StjFMSNull() { }

  StjTowerEnergyList getEnergyList() { return StjTowerEnergyList(); }

  ClassDef(StjFMSNull, 1)

};

#endif // STJFMSNULL_H
