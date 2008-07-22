// -*- mode: c++;-*-
// $Id: StJetMCMuDst.h,v 1.2 2008/07/22 05:11:42 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCMUDST_H
#define STJETMCMUDST_H

#include <StJetMC.h>

class StMCAsymMaker;
class StMcEventMaker;

namespace StSpinJet {

class StJetMCMuDst : public StJetMC {

public:

  StJetMCMuDst(StMCAsymMaker* sim,  StMcEventMaker* mc)
    : mSimuMaker(sim), mMcEventMaker(mc) { }
  virtual ~StJetMCMuDst() { }

  MCParticleList getMCPartilceList();

private:

  StMCAsymMaker* mSimuMaker;
  StMcEventMaker* mMcEventMaker;

};

}

#endif // STJETMCMUDST_H
