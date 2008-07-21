// -*- mode: c++;-*-
// $Id: JetCutPt.h,v 1.1 2008/07/21 17:24:43 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTPT_H
#define JETCUTPT_H

#include "JetCut.h"

namespace StJetJetCut {

class JetCutPt : public JetCut {

public:
  JetCutPt(double min = 0, double max = std::numeric_limits<double>::max())
    : _min(min), _max(max) { }
  virtual ~JetCutPt() { }

  bool operator()(const StSpinJet::Jet& jet)
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

#endif // JETCUTPT_H
