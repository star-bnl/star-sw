// -*- mode: c++;-*-
// $Id: StjFourVecCutPt.h,v 1.2 2008/08/02 19:22:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUTPT_H
#define FOURVECCUTPT_H

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

#endif // FOURVECCUTPT_H
