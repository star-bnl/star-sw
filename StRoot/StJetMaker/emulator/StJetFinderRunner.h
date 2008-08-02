// -*- mode: c++;-*-
// $Id: StJetFinderRunner.h,v 1.2 2008/08/02 22:43:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFINDERRUNNER_H
#define STJETFINDERRUNNER_H

#include <StJetFinder/StProtoJet.h>

#include <list>
#include <vector>

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class StJetFinderRunner {

public:

  typedef std::list<StProtoJet> ProtoJetList;
  typedef std::vector<const AbstractFourVec*> ParticleList;

  StJetFinderRunner(StJetPars* jp, const ParticleList& particleList, ProtoJetList& protoJets);

  virtual ~StJetFinderRunner();

  void Init();

  void Run();

private:

  StJetFinder* _jetFinder;

  const ParticleList& _particleList;

  ProtoJetList& _protoJetList;

};

}

#endif // STJETFINDERRUNNER_H

