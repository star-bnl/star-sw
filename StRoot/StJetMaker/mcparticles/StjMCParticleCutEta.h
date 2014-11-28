// -*- mode: c++;-*-
// $Id: StjMCParticleCutEta.h,v 1.1 2008/11/27 07:40:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUTETA_H
#define STJMCPARTICLECUTETA_H

#include "StjMCParticleCut.h"

class StjMCParticleCutEta : public StjMCParticleCut {

public:
  StjMCParticleCutEta(double min = -10.0, double max = 10.0) :_min(min), _max(max) { }
  virtual ~StjMCParticleCutEta() { }

  bool operator()(const StjMCParticle& p4)
  {
    if(p4.eta < _min) return true;

    if(p4.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  ClassDef(StjMCParticleCutEta, 1)

};

#endif // STJMCPARTICLECUTETA_H
