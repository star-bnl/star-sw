// $Id: StjTrgSoftwareTriggerSimuMaker.cxx,v 1.1 2008/08/02 22:21:33 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgSoftwareTriggerSimuMaker.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

bool StjTrgSoftwareTriggerSimuMaker::soft(int trgId)
{
  return _simuTrig->isTrigger(trgId);
}

std::vector<int> StjTrgSoftwareTriggerSimuMaker::towers(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  std::copy(towerId.begin(), towerId.end(), back_inserter(ret));

  std::sort(ret.begin(), ret.end());
  return ret;
}

std::vector<int> StjTrgSoftwareTriggerSimuMaker::jetPatches(int trgId)
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(trgId);

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  std::copy(jpId.begin(), jpId.end(), back_inserter(ret));

  std::sort(ret.begin(), ret.end());
  return ret;
}
