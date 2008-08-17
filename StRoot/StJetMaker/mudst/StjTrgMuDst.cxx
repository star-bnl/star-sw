// $Id: StjTrgMuDst.cxx,v 1.2 2008/08/17 11:29:14 tai Exp $
#include "StjTrgMuDst.h"

#include "StjTrgMuDstSoftware.h"
#include "StjTrgPassCondition.h"
#include "StjTrgBEMCJetPatchTowerIdMap.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

ClassImp(StjTrgMuDst)

using namespace std;

int StjTrgMuDst::runNumber()
{
  return _uDstMaker->muDst()->event()->runId();
}

int StjTrgMuDst::eventId()
{
  return _uDstMaker->muDst()->event()->eventId();
}

bool StjTrgMuDst::hard() const
{
  return _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId);
}

bool StjTrgMuDst::soft() const
{
  return _soft->soft(_trgId);
}

bool StjTrgMuDst::pass()
{
  return (*_passCondition)(this);
}

double StjTrgMuDst::prescale()
{
  return StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];
}

double StjTrgMuDst::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

vector<int> StjTrgMuDst::towers()
{
  return _soft->towers(_trgId);
}

vector<int> StjTrgMuDst::towerDsmAdc()
{
  return _soft->towerDsmAdc(_trgId);
}

vector<unsigned int> StjTrgMuDst::towerAdc()
{
  vector<unsigned int> ret;
  vector<int> towers = _soft->towers(_trgId);
  for(vector<int>::const_iterator tower = towers.begin(); tower != towers.end(); ++tower) {
    if(isThereTowerEnergyFor(*tower)) {
      StjTowerEnergy towerEnergy = findTowerEnergyFor(*tower);
      ret.push_back(towerEnergy.adc);
    } else {
      ret.push_back(0);
    }
  }
  return ret;
}

vector<double> StjTrgMuDst::towerEnergy()
{
  vector<double> ret;
  vector<int> towers = _soft->towers(_trgId);
  for(vector<int>::const_iterator tower = towers.begin(); tower != towers.end(); ++tower) {
    if(isThereTowerEnergyFor(*tower)) {
      StjTowerEnergy towerEnergy = findTowerEnergyFor(*tower);
      ret.push_back(towerEnergy.energy);
    } else {
      ret.push_back(0);
    }
  }
  return ret;
}

bool StjTrgMuDst::isThereTowerEnergyFor(int towerId)
{
  StjTowerEnergyList energyList = _bemc->getEnergyList();
  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
      if((*it).towerId == towerId) return true;
  }
  return false;
}

StjTowerEnergy StjTrgMuDst::findTowerEnergyFor(int towerId)
{
  StjTowerEnergyList energyList = _bemc->getEnergyList();
  for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
    if((*it).towerId == towerId) return *it;
  }
  return StjTowerEnergy();
}


vector<int> StjTrgMuDst::jetPatches()
{
  return _soft->jetPatches(_trgId);
}

vector<int> StjTrgMuDst::jetPatchDsmAdc()
{
  return _soft->jetPatchDsmAdc(_trgId);
}

vector<unsigned int> StjTrgMuDst::jetPatchAdc()
{
  vector<unsigned int> ret;
  vector<int> jetPatches = _soft->jetPatches(_trgId);
  StjTowerEnergyList energyList = _bemc->getEnergyList();
  for(vector<int>::const_iterator jp = jetPatches.begin(); jp != jetPatches.end(); ++jp) {
    unsigned int adc = 0;
    for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
      if(*jp == _bemcJpTowerMap->getJetPatchIdForTower((*it).towerId)) {
	if((*it).status == 1) adc += (*it).adc;
      }
    }
    ret.push_back(adc);
  }
  return ret;
}

vector<double> StjTrgMuDst::jetPatchEnergy()
{
  vector<double> ret;
  vector<int> jetPatches = _soft->jetPatches(_trgId);
  StjTowerEnergyList energyList = _bemc->getEnergyList();
  for(vector<int>::const_iterator jp = jetPatches.begin(); jp != jetPatches.end(); ++jp) {
    double energy = 0;
    for(StjTowerEnergyList::const_iterator it = energyList.begin(); it != energyList.end(); ++it) {
      if(*jp == _bemcJpTowerMap->getJetPatchIdForTower((*it).towerId)) {
	if((*it).status == 1) energy += (*it).energy;
      }
    }
    ret.push_back(energy);
  }
  return ret;
}
