// $Id: StjFourVecCutTrgBHT.cxx,v 1.1 2008/11/27 07:29:50 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecCutTrgBHT.h"

#include <StjTrg.h>

#include <vector>
#include <algorithm>

ClassImp(StjFourVecCutTrgBHT)

using namespace std;

bool StjFourVecCutTrgBHT::operator()(const StjFourVec& p4)
{
  if(p4.type != 2) return true;
  if(p4.detectorId != 9) return true;
  vector<int> towers = _trg->towers();
  vector<int>::const_iterator it = find(towers.begin(), towers.end(), p4.towerId);
  if(it == towers.end()) return true;
  return false;
}
