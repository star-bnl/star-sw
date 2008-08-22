// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.5 2008/08/22 17:32:54 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCMUDST_H
#define STJMCMUDST_H

#include <StjMC.h>

class StMaker;

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StjMCParticleList getMCParticleList();

private:

  StMaker* _maker;

};

#endif // STJMCMUDST_H
