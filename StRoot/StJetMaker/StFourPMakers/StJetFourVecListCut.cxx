// $Id: StJetFourVecListCut.cxx,v 1.1 2008/07/17 19:06:24 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetFourVecListCut.h"

using namespace std;

namespace StSpinJet {

FourVecList StJetFourVecListCut::operator()(const FourVecList &fourList)
{
  FourVecList ret;

  for(FourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StJetFourVecListCut::shouldNotKeep(const FourVec& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}


}
