// -*- mode: c++;-*-
// $Id: StjMC.h,v 1.2 2009/12/09 05:12:17 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMC_H
#define STJMC_H

#include <TObject.h>
#include <StThreeVectorF.hh>
#include <StjMCParticleList.h>

class StjMC : public TObject {

public:
  StjMC() { }
  virtual ~StjMC() { }

  virtual void Init() { }

  virtual StThreeVectorF getMCVertex() const = 0;
  virtual StjMCParticleList getMCParticleList() = 0;

  ClassDef(StjMC, 1)

};

#endif // STJMC_H
