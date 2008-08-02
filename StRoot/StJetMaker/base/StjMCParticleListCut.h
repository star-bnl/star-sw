// -*- mode: c++;-*-
// $Id: StjMCParticleListCut.h,v 1.3 2008/08/02 22:43:18 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLELISTCUT_H
#define STJMCPARTICLELISTCUT_H

#include "StjMCParticleCut.h"

#include "StjMCParticleList.h"

namespace StSpinJet {

class StjMCParticleListCut {

public:
  StjMCParticleListCut() { }
  virtual ~StjMCParticleListCut() { }
  
  StjMCParticleList operator()(const StjMCParticleList& aList);

  void addCut(StJetMCParticleCut::StjMCParticleCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StJetMCParticleCut::StjMCParticleCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjMCParticle& p);

  CutList _cutList;

};

}

#endif // STJMCPARTICLELISTCUT_H
