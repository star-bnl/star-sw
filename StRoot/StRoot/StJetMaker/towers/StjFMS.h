// -*- mode: c++;-*-
// $Id: StjFMS.h,v 1.1 2017/05/22 19:36:06 zchang Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFMS_H
#define STJFMS_H

#include <TObject.h>

#include "StjTowerEnergyList.h"

class StjFMS : public TObject {

public:
  StjFMS() { }
  virtual ~StjFMS() { }

  virtual void Init() { }

  virtual StjTowerEnergyList getEnergyList() = 0;

  ClassDef(StjFMS, 1)

};

#endif // STJFMS_H
