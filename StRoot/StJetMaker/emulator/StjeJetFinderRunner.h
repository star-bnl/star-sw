// -*- mode: c++;-*-
// $Id: StjeJetFinderRunner.h,v 1.1 2008/08/02 23:10:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJETFINDERRUNNER_H
#define STJETFINDERRUNNER_H

#include <StJetFinder/StProtoJet.h>

#include <list>
#include <vector>

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class StjeJetFinderRunner {

public:

  typedef std::list<StProtoJet> ProtoJetList;
  typedef std::vector<const AbstractFourVec*> ParticleList;

  StjeJetFinderRunner(StJetPars* jp, const ParticleList& particleList, ProtoJetList& protoJets);

  virtual ~StjeJetFinderRunner();

  void Init();

  void Run();

private:

  StJetFinder* _jetFinder;

  const ParticleList& _particleList;

  ProtoJetList& _protoJetList;

};

}

#endif // STJETFINDERRUNNER_H

