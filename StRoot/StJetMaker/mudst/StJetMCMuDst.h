// -*- mode: c++;-*-
// $Id: StJetMCMuDst.h,v 1.3 2008/07/22 05:48:38 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCMUDST_H
#define STJETMCMUDST_H

#include <StJetMC.h>

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
