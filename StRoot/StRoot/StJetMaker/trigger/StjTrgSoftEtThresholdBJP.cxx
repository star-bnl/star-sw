// $Id: StjTrgSoftEtThresholdBJP.cxx,v 1.5 2008/09/17 18:43:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgSoftEtThresholdBJP.h"

#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyPrint.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyCutEnergy.h"
#include "StjTowerEnergyCutEt.h"
#include "StjTowerEnergyCutAdc.h"

#include "StjTrgBEMCJetPatchTowerIdMap.h"

#include "StjBEMC.h"

#include "StjTrg.h"

#include <TVector3.h>
#include <TMath.h>

#include <iostream>
#include <map>

ClassImp(StjTrgSoftEtThresholdBJP)

using namespace std;

StjTrgSoftEtThresholdBJP::StjTrgSoftEtThresholdBJP(StjBEMC* bemc, StjTrgBEMCJetPatchTowerIdMap* jpTowerMap, double minEt)
  : _bemc(bemc), _jpTowerMap(jpTowerMap), _minEt(minEt), _runNumber(-1), _eventId(-1)
{
  _cut.addCut(  new StjTowerEnergyCutEnergy(0.0)   );
  _cut.addCut(  new StjTowerEnergyCutBemcStatus(1) );
  _cut.addCut(  new StjTowerEnergyCutAdc(0, 2.0)   );
  _cut.addCut(  new StjTowerEnergyCutEt(0.2)       );
}

bool StjTrgSoftEtThresholdBJP::isNewEvent()
{
  if(_runNumber != _trg->runNumber()) return true;
  if(_eventId != _trg->eventId()) return true;
  return false;
}

void StjTrgSoftEtThresholdBJP::read()
{
  _runNumber = _trg->runNumber();
  _eventId = _trg->eventId();

  _jetPatches.clear();
  _jetPatchDsmAdc.clear();
  _jetPatchAdc.clear();
  _jetPatchEnergy.clear();
  _jetPatchEt.clear();

  StjTowerEnergyList energyList = _bemc->getEnergyList();
  energyList = _cut(energyList);

  map<int, StjTowerEnergyList> jpMap;

  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    int jpid = _jpTowerMap->getJetPatchIdForTower((*it).towerId);
    if(jpid == -1) continue;
    jpMap[jpid].push_back(*it);
  }

  for(map<int, StjTowerEnergyList>::const_iterator it = jpMap.begin(); it != jpMap.end(); ++it) {
    int jpid = (*it).first;
    const StjTowerEnergyList& energyList = (*it).second;
    double Et = computeEtSum(energyList);
    if(Et <= _minEt) continue;
    _jetPatches.push_back(jpid);
    _jetPatchDsmAdc.push_back(0);
    _jetPatchAdc.push_back(0);
    _jetPatchEnergy.push_back(0);
    _jetPatchEt.push_back(Et);
  }

  _passed = ( ! _jetPatches.empty() );

}

double StjTrgSoftEtThresholdBJP::computeEtSum(const StjTowerEnergyList& energyList)
{
  double ret = 0;
  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    TVector3 vec3;
    vec3.SetPtEtaPhi((*it).towerR, (*it).towerEta, (*it).towerPhi);
    ret += ((*it).energy)*TMath::Sin(vec3.Theta());
  }
  return ret;
}

bool StjTrgSoftEtThresholdBJP::soft()
{
  if(isNewEvent()) read();
  return _passed;
}

vector<int> StjTrgSoftEtThresholdBJP::jetPatches()
{
  if(isNewEvent()) read();
  return _jetPatches;
}

vector<int> StjTrgSoftEtThresholdBJP::jetPatchDsmAdc()
{
  if(isNewEvent()) read();
  return _jetPatchDsmAdc;
}

vector<unsigned int> StjTrgSoftEtThresholdBJP::jetPatchAdc()
{
  if(isNewEvent()) read();
  return _jetPatchAdc;
}

vector<double> StjTrgSoftEtThresholdBJP::jetPatchEnergy()
{
  if(isNewEvent()) read();
  return _jetPatchEnergy;
}

vector<double> StjTrgSoftEtThresholdBJP::jetPatchEt()
{
  if(isNewEvent()) read();
  return _jetPatchEt;
}


