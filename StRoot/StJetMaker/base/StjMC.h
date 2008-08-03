// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.4 2008/08/03 00:26:30 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMC_H
#define STJMC_H

#include <StjMCParticleList.h>

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

#endif // STJMC_H
