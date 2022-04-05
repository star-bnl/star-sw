// -*- mode: c++;-*-
// $Id: StjeParticleCollector.h,v 1.3 2010/04/24 04:15:35 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STPARTICLECOLLECTOR_H
#define STPARTICLECOLLECTOR_H

#include "StppAnaPars.h"

#include <list>

class AbstractFourVec;
class StFourPMaker;
class StMuTrackFourVec;

class StjeParticleCollector {

public:

  typedef std::vector<const AbstractFourVec*> ParticleList;

  StjeParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ParticleList& particleList);

  virtual ~StjeParticleCollector() {}

  void Do(int iVertex);
  size_t numberOfVertices() const;

private:

  bool shoudNotPassToJetFinder(const AbstractFourVec* particle) const;

  bool isChargedTrack(const StMuTrackFourVec* p) const;

  StFourPMaker* _fourPMaker;
  ParticleList& _particleList;

  StppAnaPars _anaPar;

};

#endif // STPARTICLECOLLECTOR_H
