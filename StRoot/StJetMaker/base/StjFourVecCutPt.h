// -*- mode: c++;-*-
// $Id: StjFourVecCutPt.h,v 1.1 2008/08/02 04:15:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUTPT_H
#define FOURVECCUTPT_H

#include "StjFourVecCut.h"

namespace StJetFourVecCut {

class FourVecCutPt : public FourVecCut {

public:
  FourVecCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~FourVecCutPt() { }

  bool operator()(const StSpinJet::FourVec& p4)
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
