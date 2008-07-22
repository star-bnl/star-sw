// -*- mode: c++;-*-
// $Id: MCParticleCutStatus.h,v 1.1 2008/07/22 06:36:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTSTATUS_H
#define MCPARTICLECUTSTATUS_H

#include "MCParticleCut.h"

namespace StJetMCParticleCut {

class MCParticleCutStatus : public MCParticleCut {

public:
  MCParticleCutStatus(int goodStatus = 1)
    : _goodStatus(goodStatus) { }
  virtual ~MCParticleCutStatus() { }

  bool operator()(const StSpinJet::MCParticle& p4)
  {
    if(p4.status != _goodStatus) return true;

    return false;
  }

private:

  int _goodStatus;
};

}

#endif // MCPARTICLECUTSTATUS_H
