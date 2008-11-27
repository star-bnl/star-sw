// -*- mode: c++;-*-
// $Id: StjMCNull.h,v 1.1 2008/11/27 07:40:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCNULL_H
#define STJMCNULL_H

#include "StjMC.h"

class StjMCNull : public StjMC {

public:
  StjMCNull() { }
  virtual ~StjMCNull() { }

  void Init() { }

  StjMCParticleList getMCPartilceList() { return StjMCParticleList(); }

  ClassDef(StjMCNull, 1)

};

#endif // STJMCNULL_H
