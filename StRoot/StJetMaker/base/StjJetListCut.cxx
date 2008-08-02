// $Id: StjJetListCut.cxx,v 1.1 2008/08/02 04:15:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetListCut.h"

using namespace std;

namespace StSpinJet {

JetList StJetJetListCut::operator()(const JetList &fourList)
{
  JetList ret;

  for(JetList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StJetJetListCut::shouldNotKeep(const Jet& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}


}
