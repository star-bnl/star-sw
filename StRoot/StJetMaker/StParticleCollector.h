// -*- mode: c++;-*-
// $Id: StParticleCollector.h,v 1.5 2008/05/02 17:07:12 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@mit.edu>
#ifndef STPARTICLECOLLECTOR_HH
#define STPARTICLECOLLECTOR_HH

#include "StppAnaPars.h"

#include <list>

class AbstractFourVec;
class StFourPMaker;
class StMuTrackFourVec;

namespace StSpinJet {

class StParticleCollector {

public:

  typedef std::vector<const AbstractFourVec*> ParticleList;

  StParticleCollector(const StppAnaPars* ap, StFourPMaker* fp, ParticleList& particleList);

  virtual ~StParticleCollector();

  void Do();

private:

  bool shoudNotPassToJetFinder(const AbstractFourVec* particle) const;

  bool isChargedTrack(const StMuTrackFourVec* p) const;

  StFourPMaker* _fourPMaker;
  ParticleList& _particleList;

  StppAnaPars _anaPar;

};

}

#endif // STPARTICLECOLLECTOR_HH
