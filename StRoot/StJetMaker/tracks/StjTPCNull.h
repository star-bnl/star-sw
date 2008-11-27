// -*- mode: c++;-*-
// $Id: StjTPCNull.h,v 1.1 2008/11/27 07:09:29 tai Exp $
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
