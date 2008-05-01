// -*- mode: c++;-*-
// $Id: StJetCuts.h,v 1.1 2008/05/01 21:55:13 tai Exp $
#ifndef STJETCUTS_HH
#define STJETCUTS_HH

#include "StppAnaPars.h"

#include <StJetFinder/StProtoJet.h>

#include <list>

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class StJetCuts {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StJetCuts(const StppAnaPars* ap, StJetPars* jp, ProtoJetList& protoJets);

  virtual ~StJetCuts();

  void Init();

  void Run();

private:

  void applyCutsOnJets();

  bool shouldNotKeep(StProtoJet &pj);
    
  StJetFinder* _jetFinder;

  ProtoJetList& _protoJetList;

  StppAnaPars _anaPar;

};

}

#endif // STJETCUTS_HH

