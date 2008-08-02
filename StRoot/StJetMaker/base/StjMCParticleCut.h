// -*- mode: c++;-*-
// $Id: StjMCParticleCut.h,v 1.2 2008/08/02 19:22:47 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUT_H
#define MCPARTICLECUT_H

#include "StjMCParticleList.h"

namespace StJetMCParticleCut {

class StjMCParticleCut {

public:
  StjMCParticleCut() { }
  virtual ~StjMCParticleCut() { }

  virtual bool operator()(const StSpinJet::StjMCParticle& p4) = 0;

private:

};

}

#endif // MCPARTICLECUT_H
