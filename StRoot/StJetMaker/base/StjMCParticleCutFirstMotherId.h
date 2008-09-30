// -*- mode: c++;-*-
// $Id: StjMCParticleCutFirstMotherId.h,v 1.1 2008/09/30 17:40:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJMCPARTICLECUTFIRSTMOTHERID_H
#define STJMCPARTICLECUTFIRSTMOTHERID_H

#include "StjMCParticleCut.h"

#include <set>

class StjMCParticleCutFirstMotherId : public StjMCParticleCut {

public:
  StjMCParticleCutFirstMotherId(int goodFirstMotherId = 1)
    : _goodFirstMotherIdSet(&goodFirstMotherId, &goodFirstMotherId + 1) { }
  StjMCParticleCutFirstMotherId(int nGoodFirstMotherIdes, int* goodFirstMotherIdes)
    : _goodFirstMotherIdSet(goodFirstMotherIdes, goodFirstMotherIdes + nGoodFirstMotherIdes) { }
  virtual ~StjMCParticleCutFirstMotherId() { }

  bool operator()(const StjMCParticle& p4)
  {
    if( _goodFirstMotherIdSet.count(p4.firstMotherId) == 0 ) return true;

    return false;
  }

private:

  std::set<int> _goodFirstMotherIdSet;
  ClassDef(StjMCParticleCutFirstMotherId, 1)

};

#endif // STJMCPARTICLECUTFIRSTMOTHERID_H
