// -*- mode: c++;-*-
// $Id: MCParticleCutStatus.h,v 1.2 2008/07/25 01:05:48 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTSTATUS_H
#define MCPARTICLECUTSTATUS_H

#include "MCParticleCut.h"

#include <set>

namespace StJetMCParticleCut {

class MCParticleCutStatus : public MCParticleCut {

public:
  MCParticleCutStatus(int goodStatus = 1)
    : _goodStatusSet(&goodStatus, &goodStatus + 1) { }
  MCParticleCutStatus(int nGoodStatuses, int* goodStatuses)
    : _goodStatusSet(goodStatuses, goodStatuses + nGoodStatuses) { }
  virtual ~MCParticleCutStatus() { }

  bool operator()(const StSpinJet::MCParticle& p4)
  {
    if( _goodStatusSet.count(p4.status) == 0 ) return true;

    return false;
  }

private:

  std::set<int> _goodStatusSet;
};

}

#endif // MCPARTICLECUTSTATUS_H
