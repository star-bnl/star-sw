// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.1 2008/11/27 07:39:59 tai Exp $
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

  virtual StjMCParticleList getMCParticleList() = 0;

  ClassDef(StjMC, 1)

};

#endif // STJMC_H
