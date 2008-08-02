// -*- mode: c++;-*-
// $Id: StjMCParticleCutEtaForStatus.h,v 1.2 2008/08/02 19:22:47 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTETAFORSTATUS_H
#define MCPARTICLECUTETAFORSTATUS_H

#include "StjMCParticleCut.h"

namespace StJetMCParticleCut {

class StjMCParticleCutEtaForStatus : public StjMCParticleCut {

public:
  StjMCParticleCutEtaForStatus(double min = -10.0, double max = 10.0, int status = 1) 
    : _min(min), _max(max), _status(status) { }
  virtual ~StjMCParticleCutEtaForStatus() { }

  bool operator()(const StSpinJet::StjMCParticle& p4)
  {
    if(p4.status != _status) return false;

    if(p4.eta < _min) return true;

    if(p4.eta > _max) return true;

    return false;
  }

private:

  double  _min;
  double  _max;

  int _status;

};

}

#endif // MCPARTICLECUTETAFORSTATUS_H
