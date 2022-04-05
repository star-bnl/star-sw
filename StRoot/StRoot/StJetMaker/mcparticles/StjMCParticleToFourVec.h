// -*- mode: c++;-*-
// $Id: StjMCParticleToFourVec.h,v 1.1 2008/11/27 07:40:07 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLETOFOURVEC_H
#define STJMCPARTICLETOFOURVEC_H

#include <TObject.h>

#include "StjFourVecList.h"

class StjMCParticle;

class StjMCParticleToFourVec : public TObject {
public:
  StjMCParticleToFourVec() { }
  virtual ~StjMCParticleToFourVec() { }
  StjFourVec operator()(const StjMCParticle& mcparticle);

private:

  ClassDef(StjMCParticleToFourVec, 1)

};

#endif // STJMCPARTICLETOFOURVEC_H
