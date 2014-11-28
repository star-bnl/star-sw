// $Id: StjTrgSoftEtThresholdBHT.cxx,v 1.4 2008/09/17 18:42:59 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgSoftEtThresholdBHT.h"

#include "StjTowerEnergyCutEt.h"
#include "StjTowerEnergyCutBemcStatus.h"
#include "StjTowerEnergyPrint.h"
#include "StjTowerEnergyCutEt.h"
#include "StjTowerEnergyCutAdc.h"

#include "StjBEMC.h"

#include "StjTrg.h"

#include <TVector3.h>
#include <TMath.h>

ClassImp(StjTrgSoftEtThresholdBHT)

using namespace std;

StjTrgSoftEtThresholdBHT::StjTrgSoftEtThresholdBHT(StjBEMC* bemc, double minEt)
  : _bemc(bemc), _minEt(minEt), _runNumber(-1), _eventId(-1)
{
  _cut.addCut(  new StjTowerEnergyCutEt(minEt));
  _cut.addCut(  new StjTowerEnergyCutBemcStatus(1));
  _cut.addCut(  new StjTowerEnergyCutAdc(0, 2.0)   );
}

bool StjTrgSoftEtThresholdBHT::isNewEvent()
{
  if(_runNumber != _trg->runNumber()) return true;
  if(_eventId != _trg->eventId()) return true;
  return false;
}

void StjTrgSoftEtThresholdBHT::read()
{
  _runNumber = _trg->runNumber();
  _eventId = _trg->eventId();

  _towers.clear();
  _towerDsmAdc.clear();
  _towerAdc.clear();
  _towerEnergy.clear();
  _towerEt.clear();

  StjTowerEnergyList energyList = _bemc->getEnergyList();
  energyList = _cut(energyList);


  _passed = ( ! energyList.empty() );

  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    _towers.push_back((*it).towerId);
    _towerDsmAdc.push_back(0);
    _towerAdc.push_back((*it).adc);
    _towerEnergy.push_back((*it).energy);
    TVector3 vec3;
    vec3.SetPtEtaPhi((*it).towerR, (*it).towerEta, (*it).towerPhi);
    double Et = ((*it).energy)*TMath::Sin(vec3.Theta());
    _towerEt.push_back(Et);
  }

}

bool StjTrgSoftEtThresholdBHT::soft()
{
  if(isNewEvent()) read();
  return _passed;
}

vector<int> StjTrgSoftEtThresholdBHT::towers()
{
  if(isNewEvent()) read();
  return _towers;
}

vector<int> StjTrgSoftEtThresholdBHT::towerDsmAdc()
{
  if(isNewEvent()) read();
  return _towerDsmAdc;
}

vector<unsigned int> StjTrgSoftEtThresholdBHT::towerAdc()
{
  if(isNewEvent()) read();
  return _towerAdc;
}

vector<double> StjTrgSoftEtThresholdBHT::towerEnergy()
{
  if(isNewEvent()) read();
  return _towerEnergy;
}

vector<double> StjTrgSoftEtThresholdBHT::towerEt()
{
  if(isNewEvent()) read();
  return _towerEt;
}


