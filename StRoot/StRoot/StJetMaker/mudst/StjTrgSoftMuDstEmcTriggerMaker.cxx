// $Id: StjTrgSoftMuDstEmcTriggerMaker.cxx,v 1.1 2008/08/18 06:37:25 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgSoftMuDstEmcTriggerMaker.h"

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <StjTrgSoftGetAdcEt.h>

#include <StjTrg.h>

ClassImp(StjTrgSoftMuDstEmcTriggerMaker)

StjTrgSoftMuDstEmcTriggerMaker::StjTrgSoftMuDstEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker, StjTrgSoftGetAdcEt* adcEt)
: _emcTrigMaker(emcTrigMaker), _adcEt(adcEt)
{

}

void StjTrgSoftMuDstEmcTriggerMaker::setTrg(StjTrg* trg)
{
  _trg = trg;
  _adcEt->setTrg(_trg);
}

bool StjTrgSoftMuDstEmcTriggerMaker::soft()
{
  return _emcTrigMaker->isTrigger(_trg->id());
}

std::vector<int> StjTrgSoftMuDstEmcTriggerMaker::towers()
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(_trg->id());
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->first);
  }
  return ret;
}

std::vector<int> StjTrgSoftMuDstEmcTriggerMaker::towerDsmAdc()
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(_trg->id());
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->second);
  }
  return ret;
}

std::vector<int> StjTrgSoftMuDstEmcTriggerMaker::jetPatches()
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(_trg->id());
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->first);
  }
  return ret;
}

std::vector<int> StjTrgSoftMuDstEmcTriggerMaker::jetPatchDsmAdc()
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(_trg->id());
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->second);
  }
  return ret;
}

std::vector<unsigned int> StjTrgSoftMuDstEmcTriggerMaker::towerAdc()
{
  return _adcEt->towerAdc();
}

std::vector<double> StjTrgSoftMuDstEmcTriggerMaker::towerEnergy()
{
  return _adcEt->towerEnergy();
}

std::vector<double> StjTrgSoftMuDstEmcTriggerMaker::towerEt()
{
  return _adcEt->towerEt();
}

std::vector<unsigned int> StjTrgSoftMuDstEmcTriggerMaker::jetPatchAdc()
{
  return _adcEt->jetPatchAdc();
}

std::vector<double> StjTrgSoftMuDstEmcTriggerMaker::jetPatchEnergy()
{
  return _adcEt->jetPatchEnergy();
}

std::vector<double> StjTrgSoftMuDstEmcTriggerMaker::jetPatchEt()
{
  return _adcEt->jetPatchEt();
}
