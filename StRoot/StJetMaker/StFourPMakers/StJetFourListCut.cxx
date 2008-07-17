// $Id: StJetFourListCut.cxx,v 1.1 2008/07/17 02:19:13 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetFourListCut.h"

using namespace std;

namespace StSpinJet {

TObjArray StJetFourListCut::operator()(const TObjArray &fourList)
{
  TObjArray ret(fourList.GetEntries());

  for(Int_t i = 0; i < fourList.GetEntries(); ++i) {
    TLorentzVectorWithId* p4 = (TLorentzVectorWithId*)fourList[i];

    if(shouldNotKeep(*p4)) continue;

    ret[i] = new TLorentzVectorWithId(*p4);
  }

  return ret;
}


bool StJetFourListCut::shouldNotKeep(const TLorentzVectorWithId& p4)
{
  for(CutList::iterator cut = _cutList.begin(); cut != _cutList.end(); ++cut){
    if((**cut)(p4)) return true;
  }

  return false;
}


}
