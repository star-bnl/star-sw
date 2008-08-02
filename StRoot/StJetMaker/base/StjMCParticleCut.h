// -*- mode: c++;-*-
// $Id: StjMCParticleCut.h,v 1.1 2008/08/02 04:15:34 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUT_H
#define MCPARTICLECUT_H

#include "StjMCParticleList.h"

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
