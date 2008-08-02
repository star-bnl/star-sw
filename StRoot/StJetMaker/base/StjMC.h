// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.2 2008/08/02 19:22:47 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMC_H
#define STJETMC_H

#include <StjMCParticleList.h>

namespace StSpinJet {

class StjMC {

public:
  StjMC() { }
  virtual ~StjMC() { }

  virtual void Init() { }

  virtual StjMCParticleList getMCPartilceList() = 0;
};


class StjMCNull : public StjMC {

public:
  StjMCNull() { }
  virtual ~StjMCNull() { }

  void Init() { }

  StjMCParticleList getMCPartilceList() { return StjMCParticleList(); }
};

}

#endif // STJETMC_H
