// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.4 2008/08/03 00:29:03 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCMUDST_H
#define STJMCMUDST_H

#include <StjMC.h>

class StMaker;

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StjMCParticleList getMCPartilceList();

private:

  StMaker* _maker;

};

#endif // STJMCMUDST_H
