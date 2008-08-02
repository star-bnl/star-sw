// -*- mode: c++;-*-
// $Id: StjMCParticleCutStatus.h,v 1.1 2008/08/02 04:15:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef MCPARTICLECUTSTATUS_H
#define MCPARTICLECUTSTATUS_H

#include "StjMCParticleCut.h"

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
