// -*- mode: c++;-*-
// $Id: StjFourVecCutPt.h,v 1.4 2008/08/03 00:26:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUTPT_H
#define STJFOURVECCUTPT_H

#include "StjFourVecCut.h"

class StjFourVecCutPt : public StjFourVecCut {

public:
  StjFourVecCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~StjFourVecCutPt() { }

  bool operator()(const StjFourVec& p4)
  {
    if(p4.pt <= _min) return true;

    if(p4.pt > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

#endif // STJFOURVECCUTPT_H
