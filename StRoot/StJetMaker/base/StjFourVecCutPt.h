// -*- mode: c++;-*-
// $Id: StjFourVecCutPt.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJFOURVECCUTPT_H
#define STJFOURVECCUTPT_H

#include "StjFourVecCut.h"

namespace StJetFourVecCut {

class StjFourVecCutPt : public StjFourVecCut {

public:
  StjFourVecCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~StjFourVecCutPt() { }

  bool operator()(const StSpinJet::StjFourVec& p4)
  {
    if(p4.pt <= _min) return true;

    if(p4.pt > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

}

#endif // STJFOURVECCUTPT_H
