// -*- mode: c++;-*-
// $Id: StjJetCutDetectorEta.h,v 1.1 2008/09/12 00:32:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJJETCUTDETECTORETA_H
#define STJJETCUTDETECTORETA_H

#include "StjJetCut.h"

class StjJetCutDetectorEta : public StjJetCut {

public:
  StjJetCutDetectorEta(double min = -10.0, double max = 10.0) :_min(min), _max(max) { }
  virtual ~StjJetCutDetectorEta() { }

  bool operator()(const StjJet& jet)
  {
    if(jet.detectorEta < _min) return true;

    if(jet.detectorEta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  ClassDef(StjJetCutDetectorEta, 1)

};

#endif // STJJETCUTDETECTORETA_H
