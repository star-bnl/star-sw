// -*- mode: c++;-*-
// $Id: StjJetCutRt.h,v 1.1 2008/09/12 22:32:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTRT_H
#define STJJETCUTRT_H

#include "StjJetCut.h"

class StjJetCutRt : public StjJetCut {

public:
  StjJetCutRt(double min = 0.0, double max = 1.0) : _min(min), _max(max) { }
  virtual ~StjJetCutRt() { }

  bool operator()(const StjJet& jet)
  {
    if(jet.neuRt < _min) return true;

    if(jet.neuRt > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  ClassDef(StjJetCutRt, 1)

};

#endif // STJJETCUTRT_H
