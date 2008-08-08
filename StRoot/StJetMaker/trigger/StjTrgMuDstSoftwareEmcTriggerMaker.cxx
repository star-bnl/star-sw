// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.cxx,v 1.1 2008/08/08 23:12:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareEmcTriggerMaker.h"

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

bool StjTrgMuDstSoftwareEmcTriggerMaker::soft(int trgId)
{
  return _emcTrigMaker->isTrigger(trgId);
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::towers(int trgId)
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(trgId);
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->first);
  }
  std::sort(ret.begin(), ret.end());
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatches(int trgId)
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(trgId);
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->first);
  }
  std::sort(ret.begin(), ret.end());
  return ret;
}
