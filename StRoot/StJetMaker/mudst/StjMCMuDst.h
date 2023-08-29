// -*- mode: c++;-*-
// $Id: StjMCMuDst.h,v 1.7 2011/01/27 16:42:44 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCMUDST_H
#define STJMCMUDST_H

#include <StjMC.h>

class StMaker;
class StarGenEvent;
class StarGenEventReader;
class StarGenParticle;

class StjMCMuDst : public StjMC {

public:

  StjMCMuDst(StMaker* maker) : _maker(maker) { }
  virtual ~StjMCMuDst() { }

  StjPrimaryVertex getMCVertex() const;
  StjMCParticleList getMCParticleList();
  void setGenEvent(StarGenEvent* ev){genEvent = ev;};

private:

  StMaker* _maker;
  StarGenEvent* genEvent;
};

#endif // STJMCMUDST_H


