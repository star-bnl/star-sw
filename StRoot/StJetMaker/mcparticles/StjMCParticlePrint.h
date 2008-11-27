// -*- mode: c++;-*-
// $Id: StjMCParticlePrint.h,v 1.1 2008/11/27 07:40:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLEPRINT_H
#define STJMCPARTICLEPRINT_H

#include <TObject.h>

#include "StjMCParticleList.h"

#include <fstream>
#include <string>

class StjMCParticlePrint : public TObject {

public:

  StjMCParticlePrint() { }
  virtual ~StjMCParticlePrint() { }

  void operator()(const StjMCParticleList& mcList);

private:

  void print(const StjMCParticle& mc);

  ClassDef(StjMCParticlePrint, 1)

};

#endif // STJMCPARTICLEPRINT_H
