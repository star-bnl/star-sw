// $Id: StjTrgDisableTowerHT.cxx,v 1.1 2008/09/21 19:11:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgDisableTowerHT.h"

#include <iostream>

ClassImp(StjTrgDisableTowerHT)

using namespace std;

void StjTrgDisableTowerHT::read() const
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
    _towers.push_back(srcTowers[i]);
    _towerDsmAdc.push_back(srcTowerDsmAdc[i]);
    _towerAdc.push_back(srcTowerAdc[i]);
    if( _badTowerIdSet.count(srcTowers[i]) ) 
      {
	_towerEnergy.push_back(0.0);
	_towerEt.push_back(0.0);
      }
    else
      {
	_towerEnergy.push_back(srcTowerEnergy[i]);
	_towerEt.push_back(srcTowerEt[i]);
      }
  }

  _passed = ( ! _towers.empty() );
}

bool StjTrgDisableTowerHT::soft() const
{
  readIfNewEvent();
  return _passed;
}

vector<int> StjTrgDisableTowerHT::towers()
{
  readIfNewEvent();
  return _towers;
}

vector<int> StjTrgDisableTowerHT::towerDsmAdc()
{
  readIfNewEvent();
  return _towerDsmAdc;
}

vector<unsigned int> StjTrgDisableTowerHT::towerAdc()
{
  readIfNewEvent();
  return _towerAdc;
}

vector<double> StjTrgDisableTowerHT::towerEnergy()
{
  readIfNewEvent();
  return _towerEnergy;
}

vector<double> StjTrgDisableTowerHT::towerEt()
{
  readIfNewEvent();
  return _towerEt;
}

