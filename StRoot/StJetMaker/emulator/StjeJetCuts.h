// -*- mode: c++;-*-
// $Id: StjeJetCuts.h,v 1.3 2008/08/03 00:26:51 tai Exp $
#ifndef STJEJETCUTS_H
#define STJEJETCUTS_H

#include "StppAnaPars.h"

#include <StJetFinder/StProtoJet.h>

#include <list>

class StJetPars;
class StJetFinder;

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

#endif // STJEJETCUTS_H

