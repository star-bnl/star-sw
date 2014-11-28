// -*- mode: c++;-*-
// $Id: StjTPC.h,v 1.2 2010/05/30 07:10:12 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPC_H
#define STJTPC_H

#include <TObject.h>

#include "StjPrimaryVertex.h"
#include "StjTrackList.h"

class StjTPC : public TObject {

public:
  StjTPC() { }
  virtual ~StjTPC() { }

  virtual void Init() { }

  virtual StjPrimaryVertex getVertex() const { return StjPrimaryVertex(); }
  virtual StjTrackList getTrackList() = 0;

  ClassDef(StjTPC, 1)

};

#endif // STJTPC_H
