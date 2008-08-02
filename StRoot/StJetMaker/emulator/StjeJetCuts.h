// -*- mode: c++;-*-
// $Id: StjeJetCuts.h,v 1.1 2008/08/02 22:21:25 tai Exp $
#ifndef STJETCUTS_HH
#define STJETCUTS_HH

#include "StppAnaPars.h"

#include <StJetFinder/StProtoJet.h>

#include <list>

class StJetPars;
class StJetFinder;

namespace StSpinJet {

class StjeJetCuts {

public:

  typedef std::list<StProtoJet> ProtoJetList;

  StjeJetCuts(const StppAnaPars* ap, ProtoJetList& protoJets);

  virtual ~StjeJetCuts();

  void Apply();

private:

  bool shouldNotKeep(StProtoJet &pj);
    
  ProtoJetList& _protoJetList;

  StppAnaPars _anaPar;

};

}

#endif // STJETCUTS_HH

