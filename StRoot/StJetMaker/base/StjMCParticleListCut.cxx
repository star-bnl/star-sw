// $Id: StjMCParticleListCut.cxx,v 1.3 2008/08/03 00:26:31 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjMCParticleListCut.h"

using namespace std;

StjMCParticleList StjMCParticleListCut::operator()(const StjMCParticleList &aList)
{
  StjMCParticleList ret;

  for(StjMCParticleList::const_iterator it = aList.begin(); it != aList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StjMCParticleListCut::shouldNotKeep(const StjMCParticle& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}
