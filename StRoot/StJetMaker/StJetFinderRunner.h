// -*- mode: c++;-*-
// $Id: StJetFinderRunner.h,v 1.5 2008/05/01 22:23:48 tai Exp $
#ifndef STJETFINDERRUNNER_HH
#define STJETFINDERRUNNER_HH

#include <StJetFinder/StProtoJet.h>

#include <list>

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class StJetFinderRunner {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StJetFinderRunner(StJetPars* jp, ProtoJetList& protoJets);

  virtual ~StJetFinderRunner();

  void Init();

  void Run();

private:

  StJetFinder* _jetFinder;

  ProtoJetList& _protoJetList;

};

}

#endif // STJETFINDERRUNNER_HH

