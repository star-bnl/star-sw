// $Id: StjJetListVariation.cxx,v 1.1 2008/09/12 22:32:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjJetListVariation.h"

ClassImp(StjJetListVariation)

using namespace std;

StjJetList StjJetListVariation::operator()(const StjJetList &inList)
{
  StjJetList ret;

  for(StjJetList::const_iterator it = inList.begin(); it != inList.end(); ++it) {
    ret.push_back(vary(*it));
  }

  return ret;
}


StjJet StjJetListVariation::vary(const StjJet& iterm)
{
  StjJet ret(iterm);

  for(VarList::iterator var = _varList.begin(); var != _varList.end(); ++var){
    ret = (**var)(ret);
  }

  return ret;
}
