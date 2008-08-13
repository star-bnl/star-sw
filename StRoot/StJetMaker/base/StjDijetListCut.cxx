// $Id: StjDijetListCut.cxx,v 1.1 2008/08/13 19:44:55 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjDijetListCut.h"

ClassImp(StjDijetListCut)

using namespace std;

StjDijetList StjDijetListCut::operator()(const StjDijetList &dijetList)
{
  StjDijetList ret;

  for(StjDijetList::const_iterator it = dijetList.begin(); it != dijetList.end(); ++it) {

    if(shouldNotKeep(*it)) continue;

    ret.push_back(*it);
  }

  return ret;
}


bool StjDijetListCut::shouldNotKeep(const StjDijet& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}
