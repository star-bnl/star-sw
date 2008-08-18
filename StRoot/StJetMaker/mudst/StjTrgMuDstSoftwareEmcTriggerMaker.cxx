// $Id: StjTrgMuDstSoftwareEmcTriggerMaker.cxx,v 1.3 2008/08/18 06:20:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareEmcTriggerMaker.h"

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <StjTrgMuDstSoftwareGetAdcEt.h>

#include <StjTrg.h>

ClassImp(StjTrgMuDstSoftwareEmcTriggerMaker)

StjTrgMuDstSoftwareEmcTriggerMaker::StjTrgMuDstSoftwareEmcTriggerMaker(StEmcTriggerMaker* emcTrigMaker, StjTrgMuDstSoftwareGetAdcEt* adcEt)
: _emcTrigMaker(emcTrigMaker), _adcEt(adcEt)
{

}

void StjTrgMuDstSoftwareEmcTriggerMaker::setTrg(StjTrg* trg)
{
  _trg = trg;
  _adcEt->setTrg(_trg);
}

bool StjTrgMuDstSoftwareEmcTriggerMaker::soft()
{
  return _emcTrigMaker->isTrigger(_trg->id());
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::towers()
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(_trg->id());
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->first);
  }
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::towerDsmAdc()
{
  std::vector<int> ret;
  std::map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(_trg->id());
  for(std::map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    ret.push_back(tower->second);
  }
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatches()
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(_trg->id());
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->first);
  }
  return ret;
}

std::vector<int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatchDsmAdc()
{
  vector<int> ret;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(_trg->id());
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    ret.push_back(jp->second);
  }
  return ret;
}

std::vector<unsigned int> StjTrgMuDstSoftwareEmcTriggerMaker::towerAdc()
{
  return _adcEt->towerAdc();
}

std::vector<double> StjTrgMuDstSoftwareEmcTriggerMaker::towerEnergy()
{
  return _adcEt->towerEnergy();
}

std::vector<double> StjTrgMuDstSoftwareEmcTriggerMaker::towerEt()
{
  return _adcEt->towerEt();
}

std::vector<unsigned int> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatchAdc()
{
  return _adcEt->jetPatchAdc();
}

std::vector<double> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatchEnergy()
{
  return _adcEt->jetPatchEnergy();
}

std::vector<double> StjTrgMuDstSoftwareEmcTriggerMaker::jetPatchEt()
{
  return _adcEt->jetPatchEt();
}
