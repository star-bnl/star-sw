// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.5 2008/08/04 06:10:23 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMC_H
#define STJMC_H

#include <TObject.h>

#include <StjMCParticleList.h>

class StjMC : public TObject {

public:
  StjMC() { }
  virtual ~StjMC() { }

  virtual void Init() { }

  virtual StjMCParticleList getMCPartilceList() = 0;

  ClassDef(StjMC, 1)

};

#endif // STJMC_H
