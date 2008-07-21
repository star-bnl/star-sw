// -*- mode: c++;-*-
// $Id: StJetCuts.h,v 1.1 2008/07/21 02:48:15 tai Exp $
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

  StJetCuts(const StppAnaPars* ap, ProtoJetList& protoJets);

  virtual ~StJetCuts();

  void Apply();

private:

  bool shouldNotKeep(StProtoJet &pj);
    
  ProtoJetList& _protoJetList;

  StppAnaPars _anaPar;

};

}

#endif // STJETCUTS_HH

