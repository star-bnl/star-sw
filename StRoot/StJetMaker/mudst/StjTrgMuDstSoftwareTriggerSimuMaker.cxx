// $Id: StjTrgMuDstSoftwareTriggerSimuMaker.cxx,v 1.3 2008/08/18 06:20:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareTriggerSimuMaker.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

#include <StjTrg.h>

#include <StjTrgMuDstSoftwareGetAdcEt.h>

ClassImp(StjTrgMuDstSoftwareTriggerSimuMaker)

StjTrgMuDstSoftwareTriggerSimuMaker::StjTrgMuDstSoftwareTriggerSimuMaker(StTriggerSimuMaker* simuTrig, StjTrgMuDstSoftwareGetAdcEt* adcEt)
: _simuTrig(simuTrig), _adcEt(adcEt)
{

}

void StjTrgMuDstSoftwareTriggerSimuMaker::setTrg(StjTrg* trg)
{
  _trg = trg;
  _adcEt->setTrg(_trg);
}

bool StjTrgMuDstSoftwareTriggerSimuMaker::soft()
{
  return _simuTrig->isTrigger(_trg->id());
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::towers()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  std::copy(towerId.begin(), towerId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::towerDsmAdc()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  for(size_t i = 0; i != towerId.size(); ++i){
    ret.push_back(trigResult.highTowerAdc(towerId[i]));
  }

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatches()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  std::copy(jpId.begin(), jpId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatchDsmAdc()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  for(size_t i = 0; i != jpId.size(); ++i){
    ret.push_back(trigResult.jetPatchAdc(jpId[i]));
  }

  return ret;
}

std::vector<unsigned int> StjTrgMuDstSoftwareTriggerSimuMaker::towerAdc()
{
  return _adcEt->towerAdc();
}

std::vector<double> StjTrgMuDstSoftwareTriggerSimuMaker::towerEnergy()
{
  return _adcEt->towerEnergy();
}

std::vector<double> StjTrgMuDstSoftwareTriggerSimuMaker::towerEt()
{
  return _adcEt->towerEt();
}

std::vector<unsigned int> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatchAdc()
{
  return _adcEt->jetPatchAdc();
}

std::vector<double> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatchEnergy()
{
  return _adcEt->jetPatchEnergy();
}

std::vector<double> StjTrgMuDstSoftwareTriggerSimuMaker::jetPatchEt()
{
  return _adcEt->jetPatchEt();
}
