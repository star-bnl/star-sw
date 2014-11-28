// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.3 2011/01/27 16:42:39 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMC_H
#define STJMC_H

#include <TObject.h>
#include <StjPrimaryVertex.h>
#include <StjMCParticleList.h>

class StjMC : public TObject {

public:
  StjMC() { }
  virtual ~StjMC() { }

  virtual void Init() { }

  virtual StjPrimaryVertex  getMCVertex() const = 0;
  virtual StjMCParticleList getMCParticleList() = 0;

  ClassDef(StjMC, 1)

};

#endif // STJMC_H
