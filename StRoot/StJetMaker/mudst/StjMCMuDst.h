// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.1 2008/08/02 04:19:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCMUDST_H
#define STJETMCMUDST_H

#include <StjMC.h>

class StMaker;

namespace StSpinJet {

class StJetMCMuDst : public StJetMC {

public:

  StJetMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StJetMCMuDst() { }

  MCParticleList getMCPartilceList();

private:

  StMaker* _maker;

};

}

#endif // STJETMCMUDST_H
