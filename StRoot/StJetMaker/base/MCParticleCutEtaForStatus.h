// -*- mode: c++;-*-
// $Id: MCParticleCutEtaForStatus.h,v 1.1 2008/07/25 01:05:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTETAFORSTATUS_H
#define MCPARTICLECUTETAFORSTATUS_H

#include "MCParticleCut.h"

namespace StJetMCParticleCut {

class MCParticleCutEtaForStatus : public MCParticleCut {

public:
  MCParticleCutEtaForStatus(double min = -10.0, double max = 10.0, int status = 1) 
    : _min(min), _max(max), _status(status) { }
  virtual ~MCParticleCutEtaForStatus() { }

  bool operator()(const StSpinJet::MCParticle& p4)
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
