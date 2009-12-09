// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.6 2009/12/09 05:12:20 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCMUDST_H
#define STJMCMUDST_H

#include <StjMC.h>

class StMaker;

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StThreeVectorF getMCVertex() const;
  StjMCParticleList getMCParticleList();

private:

  StMaker* _maker;

};

#endif // STJMCMUDST_H
