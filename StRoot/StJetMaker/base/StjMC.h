// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.1 2008/08/02 04:15:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMC_H
#define STJETMC_H

#include <StjMCParticleList.h>

namespace StSpinJet {

class StJetMC {

public:
  StJetMC() { }
  virtual ~StJetMC() { }

  virtual void Init() { }

  virtual MCParticleList getMCPartilceList() = 0;
};


class StJetMCNull : public StJetMC {

public:
  StJetMCNull() { }
  virtual ~StJetMCNull() { }

  void Init() { }

  MCParticleList getMCPartilceList() { return MCParticleList(); }
};

}

#endif // STJETMC_H
