// -*- mode: c++;-*-
// $Id: MCParticleCutEta.h,v 1.2 2008/07/25 01:05:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTETA_H
#define MCPARTICLECUTETA_H

#include "MCParticleCut.h"

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
