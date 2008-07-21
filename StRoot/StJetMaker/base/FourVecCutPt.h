// -*- mode: c++;-*-
// $Id: FourVecCutPt.h,v 1.1 2008/07/21 17:24:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURVECCUTPT_H
#define FOURVECCUTPT_H

#include "FourVecCut.h"

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
