// -*- mode: c++;-*-
// $Id: StjMCParticleCutEtaForStatus.h,v 1.1 2008/11/27 07:40:02 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUTETAFORSTATUS_H
#define STJMCPARTICLECUTETAFORSTATUS_H

#include "StjMCParticleCut.h"

class StjMCParticleCutEtaForStatus : public StjMCParticleCut {

public:
  StjMCParticleCutEtaForStatus(double min = -10.0, double max = 10.0, int status = 1) 
    : _min(min), _max(max), _status(status) { }
  virtual ~StjMCParticleCutEtaForStatus() { }

  bool operator()(const StjMCParticle& p4)
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

  ClassDef(StjMCParticleCutEtaForStatus, 1)

};

#endif // STJMCPARTICLECUTETAFORSTATUS_H
