// -*- mode: c++;-*-
// $Id: StjTPC.h,v 1.3 2008/08/02 22:43:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPC_H
#define STJTPC_H

#include "StjTrackList.h"

namespace StSpinJet {

class StjTPC {

public:
  StjTPC() { }
  virtual ~StjTPC() { }

  virtual void Init() { }

  virtual StjTrackList getTrackList() = 0;
};


class StjTPCNull : public StjTPC {

public:
  StjTPCNull() { }
  virtual ~StjTPCNull() { }

  void Init() { }

  StjTrackList getTrackList() { return StjTrackList(); }
};

}

#endif // STJTPC_H
