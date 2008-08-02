// $Id: StjJetListCut.cxx,v 1.2 2008/08/02 19:22:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetListCut.h"

using namespace std;

namespace StSpinJet {

StjJetList StjJetListCut::operator()(const StjJetList &fourList)
{
  StjJetList ret;

  for(StjJetList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StjJetListCut::shouldNotKeep(const StjJet& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}


}
