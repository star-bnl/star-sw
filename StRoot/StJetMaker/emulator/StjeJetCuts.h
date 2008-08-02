// -*- mode: c++;-*-
// $Id: StjeJetCuts.h,v 1.2 2008/08/02 22:33:28 tai Exp $
#ifndef STJEJETCUTS_H
#define STJEJETCUTS_H

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

#endif // STJEJETCUTS_H

