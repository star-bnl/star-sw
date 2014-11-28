// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.7 2011/01/27 16:42:44 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCMUDST_H
#define STJMCMUDST_H

#include <StjMC.h>

class StMaker;

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StjPrimaryVertex getMCVertex() const;
  StjMCParticleList getMCParticleList();

private:

  StMaker* _maker;

};

#endif // STJMCMUDST_H
