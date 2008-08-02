// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.2 2008/08/02 19:23:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCMUDST_H
#define STJETMCMUDST_H

#include <StjMC.h>

class StMaker;

namespace StSpinJet {

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StjMCParticleList getMCPartilceList();

private:

  StMaker* _maker;

};

}

#endif // STJETMCMUDST_H
