// -*- mode: c++;-*-
// $Id: StjMCParticleCutStatus.h,v 1.5 2008/08/04 06:10:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUTSTATUS_H
#define STJMCPARTICLECUTSTATUS_H

#include "StjMCParticleCut.h"

#include <set>

class StjMCParticleCutStatus : public StjMCParticleCut {

public:
  StjMCParticleCutStatus(int goodStatus = 1)
    : _goodStatusSet(&goodStatus, &goodStatus + 1) { }
  StjMCParticleCutStatus(int nGoodStatuses, int* goodStatuses)
    : _goodStatusSet(goodStatuses, goodStatuses + nGoodStatuses) { }
  virtual ~StjMCParticleCutStatus() { }

  bool operator()(const StjMCParticle& p4)
  {
    if( _goodStatusSet.count(p4.status) == 0 ) return true;

    return false;
  }

private:

  std::set<int> _goodStatusSet;
  ClassDef(StjMCParticleCutStatus, 1)

};

#endif // STJMCPARTICLECUTSTATUS_H
