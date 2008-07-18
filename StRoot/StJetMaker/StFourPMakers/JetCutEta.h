// -*- mode: c++;-*-
// $Id: JetCutEta.h,v 1.1 2008/07/18 01:39:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef JETCUTETA_H
#define JETCUTETA_H

#include "JetCut.h"

namespace StJetJetCut {

class JetCutEta : public JetCut {

public:
  JetCutEta(double min = -10.0, double max = 10.0) :_min(min), _max(max) { }
  virtual ~JetCutEta() { }

  bool operator()(const StSpinJet::Jet& jet)
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
