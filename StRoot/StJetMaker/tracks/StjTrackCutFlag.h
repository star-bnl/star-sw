// -*- mode: c++;-*-
// $Id: StjTrackCutFlag.h,v 1.1 2008/11/27 07:09:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTRACKCUTFLAG_H
#define STJTRACKCUTFLAG_H

#include "StjTrackCut.h"

class StjTrackCutFlag : public StjTrackCut {

public:
  StjTrackCutFlag(Short_t min = 0,
		  Short_t max = kMaxShort)
    : _min(min), _max(max) { }
  virtual ~StjTrackCutFlag() { }

  bool operator()(const StjTrack& track)
  {
    if(track.flag <= _min) return true;

    if(track.flag > _max) return true;

    return false;
  }

private:

  Short_t _min;
  Short_t _max;

  ClassDef(StjTrackCutFlag, 1)

};

#endif // STJTRACKCUTFLAG_H
