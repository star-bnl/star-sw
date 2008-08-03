// -*- mode: c++;-*-
// $Id: StjMCParticleCut.h,v 1.4 2008/08/03 00:26:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUT_H
#define STJMCPARTICLECUT_H

#include "StjMCParticleList.h"

class StjMCParticleCut {

public:
  StjMCParticleCut() { }
  virtual ~StjMCParticleCut() { }

  virtual bool operator()(const StjMCParticle& p4) = 0;

private:

};

#endif // STJMCPARTICLECUT_H
