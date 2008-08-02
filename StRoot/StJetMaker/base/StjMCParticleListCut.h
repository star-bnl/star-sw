// -*- mode: c++;-*-
// $Id: StjMCParticleListCut.h,v 1.1 2008/08/02 04:15:39 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCPARTICLELISTCUT_H
#define STJETMCPARTICLELISTCUT_H

#include "StjMCParticleCut.h"

#include "StjMCParticleList.h"

namespace StSpinJet {

class StJetMCParticleListCut {

public:
  StJetMCParticleListCut() { }
  virtual ~StJetMCParticleListCut() { }
  
  MCParticleList operator()(const MCParticleList& aList);

  void addCut(StJetMCParticleCut::MCParticleCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetMCParticleCut::MCParticleCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const MCParticle& p);

  CutList _cutList;

};

}

#endif // STJETMCPARTICLELISTCUT_H
