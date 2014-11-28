// -*- mode: c++;-*-
// $Id: StjJetCutEta.h,v 1.1 2008/09/12 00:32:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTETA_H
#define STJJETCUTETA_H

#include "StjJetCut.h"

class StjJetCutEta : public StjJetCut {

public:
  StjJetCutEta(double min = -10.0, double max = 10.0) : _min(min), _max(max) { }
  virtual ~StjJetCutEta() { }

  bool operator()(const StjJet& jet)
  {
    if(jet.eta < _min) return true;

    if(jet.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  ClassDef(StjJetCutEta, 1)

};

#endif // STJJETCUTETA_H
