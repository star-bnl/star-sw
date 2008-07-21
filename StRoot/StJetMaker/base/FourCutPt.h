// -*- mode: c++;-*-
// $Id: FourCutPt.h,v 1.1 2008/07/21 17:24:40 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef FOURCUTPT_H
#define FOURCUTPT_H

#include "FourCut.h"

namespace StJetFourCut {

class FourCutPt : public FourCut {

public:
  FourCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~FourCutPt() { }

  bool operator()(const TLorentzVectorWithId& p4)
  {
    if(p4.Pt() <= _min) return true;

    if(p4.Pt() > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

}

#endif // FOURCUTPT_H
