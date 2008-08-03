// -*- mode: c++;-*-
// $Id: StjTPC.h,v 1.5 2008/08/03 22:04:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPC_H
#define STJTPC_H

#include <TObject.h>

#include "StjTrackList.h"

class StjTPC : public TObject {

public:
  StjTPC() { }
  virtual ~StjTPC() { }

  virtual void Init() { }

  virtual StjTrackList getTrackList() = 0;

  ClassDef(StjTPC, 1)

};

#endif // STJTPC_H
