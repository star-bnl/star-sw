// $Id: StjFourVecCutTrgBJP.cxx,v 1.1 2008/11/27 07:29:51 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjFourVecCutTrgBJP.h"

#include <StjTrgBEMCJetPatchTowerIdMap.h>

#include <StjTrg.h>

#include <vector>
#include <algorithm>
#include <iostream>

ClassImp(StjFourVecCutTrgBJP)

using namespace std;

bool StjFourVecCutTrgBJP::operator()(const StjFourVec& p4)
{
  if(p4.type != 2) return true;
  if(p4.detectorId != 9) return true;
  int jp = _jetPatchTowerMap->getJetPatchIdForTower(p4.towerId);
  vector<int> jetPatches = _trg->jetPatches();
  vector<int>::const_iterator it = find(jetPatches.begin(), jetPatches.end(), jp);
  if(it == jetPatches.end()) return true;
  return false;
}
