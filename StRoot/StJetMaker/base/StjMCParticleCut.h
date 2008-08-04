// -*- mode: c++;-*-
// $Id: StjMCParticleCut.h,v 1.5 2008/08/04 06:10:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUT_H
#define STJMCPARTICLECUT_H

#include <TObject.h>

#include "StjMCParticleList.h"

class StjMCParticleCut : public TObject {

public:
  StjMCParticleCut() { }
  virtual ~StjMCParticleCut() { }

  virtual bool operator()(const StjMCParticle& p4) = 0;

private:

  ClassDef(StjMCParticleCut, 1)

};

#endif // STJMCPARTICLECUT_H
