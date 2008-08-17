// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.cxx,v 1.2 2008/08/17 11:29:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareEmcTriggerMaker.h"

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

ClassImp(StjTrgMuDstSoftwareEmcTriggerMaker)

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
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::towerDsmAdc(int trgId)
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(trgId);
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->second);
  }
  return ret;
}


std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatches(int trgId)
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(trgId);
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->first);
  }
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatchDsmAdc(int trgId)
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(trgId);
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->second);
  }
  return ret;
}
