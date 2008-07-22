// -*- mode: c++;-*-
// $Id: MCParticleCut.h,v 1.1 2008/07/22 06:36:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUT_H
#define MCPARTICLECUT_H

#include "MCParticleList.h"

namespace StJetMCParticleCut {

class MCParticleCut {

public:
  MCParticleCut() { }
  virtual ~MCParticleCut() { }

  virtual bool operator()(const StSpinJet::MCParticle& p4) = 0;

private:

};

}

#endif // MCPARTICLECUT_H
