// -*- mode: c++;-*-
// $Id: StjDijetCutSameSideRt.h,v 1.1 2008/09/12 22:32:53 tai Exp $
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
    if(_jetcut(dijet.jet3)) return true;

    if(_jetcut(dijet.jet4)) return true;

    return false;
  }

private:

  StjJetCutRt _jetcut;

  ClassDef(StjDijetCutSameSideRt, 1)

};

#endif // STJDIJETCUTSAMESIDERT_H
