// -*- mode: c++;-*-
// $Id: StjeParticleCollector.h,v 1.1 2008/08/02 23:10:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STPARTICLECOLLECTOR_H
#define STPARTICLECOLLECTOR_H

#include "StppAnaPars.h"

#include <list>

class AbstractFourVec;
class StFourPMaker;
class StMuTrackFourVec;

namespace StSpinJet {

class StjeParticleCollector {

public:

  typedef std::vector<const AbstractFourVec*> ParticleList;

  StjeParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ParticleList& particleList);

  virtual ~StjeParticleCollector();

  void Do();

private:

  bool shoudNotPassToJetFinder(const AbstractFourVec* particle) const;

  bool isChargedTrack(const StMuTrackFourVec* p) const;

  StFourPMaker* _fourPMaker;
  ParticleList& _particleList;

  StppAnaPars _anaPar;

};

}

#endif // STPARTICLECOLLECTOR_H
