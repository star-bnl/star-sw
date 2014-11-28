// $Id: StjTrgRaiseThresholdEtJP.cxx,v 1.2 2008/08/21 22:23:04 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgRaiseThresholdEtJP.h"

#include <iostream>

ClassImp(StjTrgRaiseThresholdEtJP)

using namespace std;

void StjTrgRaiseThresholdEtJP::read() const
{
  _jetPatches.clear();
  _jetPatchDsmAdc.clear();
  _jetPatchAdc.clear();
  _jetPatchEnergy.clear();
  _jetPatchEt.clear();

  if( ! _src->soft() ) {
    _passed = false;
    return;
  }

  vector<int>          srcJetPatchs      = _src->jetPatches();    
  vector<int>          srcJetPatchDsmAdc = _src->jetPatchDsmAdc();
  vector<unsigned int> srcJetPatchAdc    = _src->jetPatchAdc();
  vector<double>       srcJetPatchEnergy = _src->jetPatchEnergy();
  vector<double>       srcJetPatchEt     = _src->jetPatchEt();

  for(size_t i = 0; i != srcJetPatchs.size(); ++i) {
    if(srcJetPatchEt[i] < _minEt) continue;

    _jetPatches.push_back(srcJetPatchs[i]);
    _jetPatchDsmAdc.push_back(srcJetPatchDsmAdc[i]);
    _jetPatchAdc.push_back(srcJetPatchAdc[i]);
    _jetPatchEnergy.push_back(srcJetPatchEnergy[i]);
    _jetPatchEt.push_back(srcJetPatchEt[i]);
  }

  _passed = ( ! _jetPatches.empty() );
}

bool StjTrgRaiseThresholdEtJP::soft() const
{
  readIfNewEvent();
  return _passed;
}

vector<int> StjTrgRaiseThresholdEtJP::jetPatches()
{
  readIfNewEvent();
  return _jetPatches;
}

vector<int> StjTrgRaiseThresholdEtJP::jetPatchDsmAdc()
{
  readIfNewEvent();
  return _jetPatchDsmAdc;
}

vector<unsigned int> StjTrgRaiseThresholdEtJP::jetPatchAdc()
{
  readIfNewEvent();
  return _jetPatchAdc;
}

vector<double> StjTrgRaiseThresholdEtJP::jetPatchEnergy()
{
  readIfNewEvent();
  return _jetPatchEnergy;
}

vector<double> StjTrgRaiseThresholdEtJP::jetPatchEt()
{
  readIfNewEvent();
  return _jetPatchEt;
}

