// $Id: StjTrgMuDstSoftwareTriggerSimuMaker.cxx,v 1.2 2008/08/17 11:29:15 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareTriggerSimuMaker.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

ClassImp(StjTrgMuDstSoftwareTriggerSimuMaker)

bool StjTrgMuDstSoftwareTriggerSimuMaker::soft(int trgId)
{
  return _simuTrig->isTrigger(trgId);
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::towers(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  std::copy(towerId.begin(), towerId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::towerDsmAdc(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  for(size_t i = 0; i != towerId.size(); ++i){
    ret.push_back(trigResult.highTowerAdc(towerId[i]));
  }

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatches(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  std::copy(jpId.begin(), jpId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatchDsmAdc(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  for(size_t i = 0; i != jpId.size(); ++i){
    ret.push_back(trigResult.jetPatchAdc(jpId[i]));
  }

  return ret;
}
