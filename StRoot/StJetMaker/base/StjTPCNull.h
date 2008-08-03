// -*- mode: c++;-*-
// $Id: StjTPCNull.h,v 1.1 2008/08/03 22:04:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTPCNULL_H
#define STJTPCNULL_H

#include "StjTPC.h"

class StjTPCNull : public StjTPC {

public:
  StjTPCNull() { }
  virtual ~StjTPCNull() { }

  void Init() { }

  StjTrackList getTrackList() { return StjTrackList(); }

  ClassDef(StjTPCNull, 1)

};

#endif // STJTPCNULL_H
