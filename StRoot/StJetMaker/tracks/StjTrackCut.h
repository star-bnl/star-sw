// -*- mode: c++;-*-
// $Id: StjTrackCut.h,v 1.2 2010/04/13 13:30:51 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUT_H
#define STJTRACKCUT_H

#include <TObject.h>

#include "StjTrackList.h"

class StjTrackCut : public TObject {

public:
  StjTrackCut() { }
  virtual ~StjTrackCut() { }

  virtual bool operator()(const StjTrack& track) const = 0;

  ClassDef(StjTrackCut, 1)

};

#endif // STJTRACKCUT_H
