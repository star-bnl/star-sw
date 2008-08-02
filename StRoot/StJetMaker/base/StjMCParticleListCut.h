// -*- mode: c++;-*-
// $Id: StjMCParticleListCut.h,v 1.2 2008/08/02 19:22:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETMCPARTICLELISTCUT_H
#define STJETMCPARTICLELISTCUT_H

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

#endif // STJETMCPARTICLELISTCUT_H
