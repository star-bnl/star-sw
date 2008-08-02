// -*- mode: c++;-*-
// $Id: StjJetCutEta.h,v 1.2 2008/08/02 19:22:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTETA_H
#define JETCUTETA_H

#include "StjJetCut.h"

namespace StJetJetCut {

class StjJetCutEta : public StjJetCut {

public:
  StjJetCutEta(double min = -10.0, double max = 10.0) :_min(min), _max(max) { }
  virtual ~StjJetCutEta() { }

  bool operator()(const StSpinJet::StjJet& jet)
  {
    if(jet.eta < _min) return true;

    if(jet.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

};

}

#endif // JETCUTETA_H
