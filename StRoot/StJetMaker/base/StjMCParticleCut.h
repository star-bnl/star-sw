// -*- mode: c++;-*-
// $Id: StjMCParticleCut.h,v 1.3 2008/08/02 22:43:17 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUT_H
#define STJMCPARTICLECUT_H

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

#endif // STJMCPARTICLECUT_H
