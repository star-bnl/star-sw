// -*- mode: c++;-*-
// $Id: StjTPC.h,v 1.2 2008/08/02 19:22:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETTPC_H
#define STJETTPC_H

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

#endif // STJETTPC_H
