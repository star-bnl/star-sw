// $Id: StjTrgSoftMuDstTriggerSimuMaker.cxx,v 1.1 2008/08/18 06:37:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgSoftMuDstTriggerSimuMaker.h"

#include "StTriggerUtilities/StTriggerSimuMaker.h"
#include "StTriggerUtilities/StTriggerSimuResult.h"

#include <StjTrg.h>

#include <StjTrgSoftGetAdcEt.h>

ClassImp(StjTrgSoftMuDstTriggerSimuMaker)

StjTrgSoftMuDstTriggerSimuMaker::StjTrgSoftMuDstTriggerSimuMaker(StTriggerSimuMaker* simuTrig, StjTrgSoftGetAdcEt* adcEt)
: _simuTrig(simuTrig), _adcEt(adcEt)
{

}

void StjTrgSoftMuDstTriggerSimuMaker::setTrg(StjTrg* trg)
{
  _trg = trg;
  _adcEt->setTrg(_trg);
}

bool StjTrgSoftMuDstTriggerSimuMaker::soft()
{
  return _simuTrig->isTrigger(_trg->id());
}

std::vector<int> StjTrgSoftMuDstTriggerSimuMaker::towers()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  std::copy(towerId.begin(), towerId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgSoftMuDstTriggerSimuMaker::towerDsmAdc()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> towerId = trigResult.highTowerIds();

  std::vector<int> ret;

  for(size_t i = 0; i != towerId.size(); ++i){
    ret.push_back(trigResult.highTowerAdc(towerId[i]));
  }

  return ret;
}

std::vector<int> StjTrgSoftMuDstTriggerSimuMaker::jetPatches()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  std::copy(jpId.begin(), jpId.end(), back_inserter(ret));

  return ret;
}

std::vector<int> StjTrgSoftMuDstTriggerSimuMaker::jetPatchDsmAdc()
{
  StTriggerSimuResult trigResult = _simuTrig->detailedResult(_trg->id());

  std::vector<short> jpId = trigResult.jetPatchIds();

  std::vector<int> ret;

  for(size_t i = 0; i != jpId.size(); ++i){
    ret.push_back(trigResult.jetPatchAdc(jpId[i]));
  }

  return ret;
}

std::vector<unsigned int> StjTrgSoftMuDstTriggerSimuMaker::towerAdc()
{
  return _adcEt->towerAdc();
}

std::vector<double> StjTrgSoftMuDstTriggerSimuMaker::towerEnergy()
{
  return _adcEt->towerEnergy();
}

std::vector<double> StjTrgSoftMuDstTriggerSimuMaker::towerEt()
{
  return _adcEt->towerEt();
}

std::vector<unsigned int> StjTrgSoftMuDstTriggerSimuMaker::jetPatchAdc()
{
  return _adcEt->jetPatchAdc();
}

std::vector<double> StjTrgSoftMuDstTriggerSimuMaker::jetPatchEnergy()
{
  return _adcEt->jetPatchEnergy();
}

std::vector<double> StjTrgSoftMuDstTriggerSimuMaker::jetPatchEt()
{
  return _adcEt->jetPatchEt();
}
