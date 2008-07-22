// -*- mode: c++;-*-
// $Id: StJetMC.h,v 1.1 2008/07/22 05:06:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMC_H
#define STJETMC_H

#include <MCParticleList.h>

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
