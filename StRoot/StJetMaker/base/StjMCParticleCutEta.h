// -*- mode: c++;-*-
// $Id: StjMCParticleCutEta.h,v 1.1 2008/08/02 04:15:35 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTETA_H
#define MCPARTICLECUTETA_H

#include "StjMCParticleCut.h"

namespace StJetMCParticleCut {

class MCParticleCutEta : public MCParticleCut {

public:
  MCParticleCutEta(double min = -10.0, double max = 10.0) :_min(min), _max(max) { }
  virtual ~MCParticleCutEta() { }

  bool operator()(const StSpinJet::MCParticle& p4)
  {
    if(p4.eta < _min) return true;

    if(p4.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

};

}

#endif // MCPARTICLECUTETA_H
