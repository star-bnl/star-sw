// $Id: StjFourVecListCut.cxx,v 1.1 2008/11/27 07:29:52 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecListCut.h"

ClassImp(StjFourVecListCut)

using namespace std;

StjFourVecList StjFourVecListCut::operator()(const StjFourVecList &fourList)
{
  StjFourVecList ret;

  for(StjFourVecList::const_iterator it = fourList.begin(); it != fourList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StjFourVecListCut::shouldNotKeep(const StjFourVec& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}
