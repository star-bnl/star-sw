// $Id: StjTrgDisableTowerJP.cxx,v 1.1 2008/09/21 19:11:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgDisableTowerJP.h"

#include "StjTrgBEMCJetPatchTowerIdMap.h"
#include "StjTowerEnergyList.h"
#include "StjBEMC.h"
#include "StjTowerEnergyCutEt.h"
#include "StjTowerEnergyListCut.h"

#include <TVector3.h>
#include <TMath.h>

#include <iostream>

ClassImp(StjTrgDisableTowerJP)

using namespace std;

void StjTrgDisableTowerJP::read() const
{
  _jetPatches.clear();
  _jetPatchDsmAdc.clear();
  _jetPatchAdc.clear();
  _jetPatchEnergy.clear();
  _jetPatchEt.clear();

  if( ! _src->soft() )
    {
      _passed = false;
      return;
    }

  vector<int>          srcJetPatchs      = _src->jetPatches();    
  vector<int>          srcJetPatchDsmAdc = _src->jetPatchDsmAdc();

  StjTowerEnergyListCut cut;
  cut.addCut(  new StjTowerEnergyCutEt(0.2)  );

  StjTowerEnergyList energyList = _bemc->getEnergyList();
  energyList = cut(energyList);

  for(size_t i = 0; i != srcJetPatchs.size(); ++i)
    {
      _jetPatches.push_back(srcJetPatchs[i]);
      _jetPatchDsmAdc.push_back(srcJetPatchDsmAdc[i]);
      unsigned int adc = 0;
      double energy = 0;
      double et = 0;
      for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it)
	{
	  if( _badTowerIdSet.count((*it).towerId) ) continue;
	  if(srcJetPatchs[i] == _jpTowerMap->getJetPatchIdForTower((*it).towerId))
	    {
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

  _passed = ( ! _jetPatches.empty() );
}

bool StjTrgDisableTowerJP::soft() const
{
  readIfNewEvent();
  return _passed;
}

vector<int> StjTrgDisableTowerJP::jetPatches()
{
  readIfNewEvent();
  return _jetPatches;
}

vector<int> StjTrgDisableTowerJP::jetPatchDsmAdc()
{
  readIfNewEvent();
  return _jetPatchDsmAdc;
}

vector<unsigned int> StjTrgDisableTowerJP::jetPatchAdc()
{
  readIfNewEvent();
  return _jetPatchAdc;
}

vector<double> StjTrgDisableTowerJP::jetPatchEnergy()
{
  readIfNewEvent();
  return _jetPatchEnergy;
}

vector<double> StjTrgDisableTowerJP::jetPatchEt()
{
  readIfNewEvent();
  return _jetPatchEt;
}

