// -*- mode: c++;-*-
// $Id: StjMCParticleListCut.h,v 1.4 2008/08/03 00:26:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLELISTCUT_H
#define STJMCPARTICLELISTCUT_H

#include "StjMCParticleCut.h"

#include "StjMCParticleList.h"

class StjMCParticleListCut {

public:
  StjMCParticleListCut() { }
  virtual ~StjMCParticleListCut() { }
  
  StjMCParticleList operator()(const StjMCParticleList& aList);

  void addCut(StjMCParticleCut* cut) {
    _cutList.push_back(cut);
  }

  typedef std::vector<StjMCParticleCut*> CutList;
  CutList getCutList() { return _cutList; }

private:

  bool shouldNotKeep(const StjMCParticle& p);

  CutList _cutList;

};

#endif // STJMCPARTICLELISTCUT_H
