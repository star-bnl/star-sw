// -*- mode: c++;-*-
// $Id: StjJetCutPt.h,v 1.3 2008/08/02 22:43:16 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTPT_H
#define STJJETCUTPT_H

#include "StjJetCut.h"

namespace StJetJetCut {

class StjJetCutPt : public StjJetCut {

public:
  StjJetCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~StjJetCutPt() { }

  bool operator()(const StSpinJet::StjJet& jet)
  {
    if(jet.pt <= _min) return true;

    if(jet.pt > _max) return true;

    return false;
  }

private:

  double _min;
  double _max;
};

}

#endif // STJJETCUTPT_H
