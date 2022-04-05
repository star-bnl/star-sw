// $Id: StjTrgRaiseThresholdEtHT.cxx,v 1.2 2008/08/21 22:23:03 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgRaiseThresholdEtHT.h"

#include <iostream>

ClassImp(StjTrgRaiseThresholdEtHT)

using namespace std;

void StjTrgRaiseThresholdEtHT::read() const
{
  _towers.clear();
  _towerDsmAdc.clear();
  _towerAdc.clear();
  _towerEnergy.clear();
  _towerEt.clear();

  if( ! _src->soft() ) {
    _passed = false;
    return;
  }

  vector<int>          srcTowers      = _src->towers();    
  vector<int>          srcTowerDsmAdc = _src->towerDsmAdc();
  vector<unsigned int> srcTowerAdc    = _src->towerAdc();
  vector<double>       srcTowerEnergy = _src->towerEnergy();
  vector<double>       srcTowerEt     = _src->towerEt();

  for(size_t i = 0; i != srcTowers.size(); ++i) {
    if(srcTowerEt[i] < _minEt) continue;

    _towers.push_back(srcTowers[i]);
    _towerDsmAdc.push_back(srcTowerDsmAdc[i]);
    _towerAdc.push_back(srcTowerAdc[i]);
    _towerEnergy.push_back(srcTowerEnergy[i]);
    _towerEt.push_back(srcTowerEt[i]);
  }

  _passed = ( ! _towers.empty() );
}

bool StjTrgRaiseThresholdEtHT::soft() const
{
  readIfNewEvent();
  return _passed;
}

vector<int> StjTrgRaiseThresholdEtHT::towers()
{
  readIfNewEvent();
  return _towers;
}

vector<int> StjTrgRaiseThresholdEtHT::towerDsmAdc()
{
  readIfNewEvent();
  return _towerDsmAdc;
}

vector<unsigned int> StjTrgRaiseThresholdEtHT::towerAdc()
{
  readIfNewEvent();
  return _towerAdc;
}

vector<double> StjTrgRaiseThresholdEtHT::towerEnergy()
{
  readIfNewEvent();
  return _towerEnergy;
}

vector<double> StjTrgRaiseThresholdEtHT::towerEt()
{
  readIfNewEvent();
  return _towerEt;
}

