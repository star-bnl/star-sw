// -*- mode: c++;-*-
// $Id: StjDijetCutSameSideRt.h,v 1.2 2008/09/13 00:03:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJDIJETCUTSAMESIDERT_H
#define STJDIJETCUTSAMESIDERT_H

#include "StjDijetCut.h"

#include "StjJetCutRt.h"

class StjDijetCutSameSideRt : public StjDijetCut {

public:
  StjDijetCutSameSideRt(double min = 0.0, double max = 1.0)
    : _jetcut(min, max) { }
  virtual ~StjDijetCutSameSideRt() { }

  bool operator()(const StjDijet& dijet)
  {
    if(_jetcut(dijet.jetSameSide)) return true;

    return false;
  }

private:

  StjJetCutRt _jetcut;

  ClassDef(StjDijetCutSameSideRt, 1)

};

#endif // STJDIJETCUTSAMESIDERT_H
