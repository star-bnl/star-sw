// $Id: StjTrgMuDstSoftwareGetAdcEt.cxx,v 1.1 2008/08/18 06:20:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgMuDstSoftwareGetAdcEt.h"

#include "StjTrgBEMCJetPatchTowerIdMap.h"

#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutEnergy.h"

#include "StjTrg.h"

#include <TVector3.h>
#include <TMath.h>

#include <iostream>

ClassImp(StjTrgMuDstSoftwareGetAdcEt)

using namespace std;

StjTrgMuDstSoftwareGetAdcEt::StjTrgMuDstSoftwareGetAdcEt(StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* bemcJpTowerMap) 
  : _bemc(bemc), _bemcJpTowerMap(bemcJpTowerMap), _trg(0)
  , _runNumber(-1), _eventId(-1)
{
  _cut.addCut(new StjTowerEnergyCutEnergy(0.0));
  _cut.addCut(new StjTowerEnergyCutBemcStatus(1));
}

bool StjTrgMuDstSoftwareGetAdcEt::isNewEvent()
{
  if(_runNumber != _trg->runNumber()) return true;
  if(_eventId != _trg->eventId()) return true;
  return false;
}

void StjTrgMuDstSoftwareGetAdcEt::read()
{
  _towerAdc.clear();
  _towerEnergy.clear();
  _towerEt.clear();

  _jetPatchAdc.clear();
  _jetPatchEnergy.clear();
  _jetPatchEt.clear();

  vector<int> towers = _trg->towers();
  vector<int> jetPatches = _trg->jetPatches();

  StjTowerEnergyList energyList = _bemc->getEnergyList();
  energyList = _cut(energyList);

  for(vector<int>::const_iterator tower = towers.begin(); tower != towers.end(); ++tower) {
    bool found = false;
    for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
      if((*it).towerId == (*tower)) {
	_towerAdc.push_back((*it).adc);
	_towerEnergy.push_back((*it).energy);
	TVector3 vec3;
	vec3.SetPtEtaPhi((*it).towerR, (*it).towerEta, (*it).towerPhi);
	_towerEt.push_back(((*it).energy)*TMath::Sin(vec3.Theta()));
	found = true;
	break;
      }
    }
    if(!found) {
      _towerAdc.push_back(0);
      _towerEnergy.push_back(0);
    }
  }

  for(vector<int>::const_iterator jp = jetPatches.begin(); jp != jetPatches.end(); ++jp) {
    unsigned int adc = 0;
    double energy = 0;
    double et = 0;
    for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
      if(*jp == _bemcJpTowerMap->getJetPatchIdForTower((*it).towerId)) {
	adc += (*it).adc;
	energy += (*it).energy;
	TVector3 vec3;
	vec3.SetPtEtaPhi((*it).towerR, (*it).towerEta, (*it).towerPhi);
	et += ((*it).energy)*TMath::Sin(vec3.Theta());
      }
    }
    _jetPatchAdc.push_back(adc);
    _jetPatchEnergy.push_back(energy);
    _jetPatchEt.push_back(et);
  }


}

vector<unsigned int> StjTrgMuDstSoftwareGetAdcEt::towerAdc()
{
  if(isNewEvent()) read();
  return _towerAdc;
}

vector<double> StjTrgMuDstSoftwareGetAdcEt::towerEnergy()
{
  if(isNewEvent()) read();
  return _towerEnergy;
}

vector<double> StjTrgMuDstSoftwareGetAdcEt::towerEt()
{
  if(isNewEvent()) read();
  return _towerEt;
}

vector<unsigned int> StjTrgMuDstSoftwareGetAdcEt::jetPatchAdc()
{
  if(isNewEvent()) read();
  return _jetPatchAdc;
}

vector<double> StjTrgMuDstSoftwareGetAdcEt::jetPatchEnergy()
{
  if(isNewEvent()) read();
  return _jetPatchEnergy;
}

vector<double> StjTrgMuDstSoftwareGetAdcEt::jetPatchEt()
{
  if(isNewEvent()) read();
  return _jetPatchEt;
}
