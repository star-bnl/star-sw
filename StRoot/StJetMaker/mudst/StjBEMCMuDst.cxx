// $Id: StjBEMCMuDst.cxx,v 1.12 2017/05/23 18:32:15 zchang Exp $
#include "StjBEMCMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StEvent/StEmcRawHit.h>
#include <StEvent/StEmcCollection.h>
#include <StEvent/StEmcModule.h>
#include <StEvent/StEmcDetector.h>
#include <StEvent/StEvent.h>

#include <StEmcUtil/geometry/StEmcGeom.h>


#include <StEmcRawMaker/defines.h>
#include <StEmcRawMaker/StBemcTables.h>

#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcSimulatorMaker/StEmcSimulatorMaker.h"

#include <TVector3.h>

#include <iostream>

using namespace std;

int StjBEMCMuDst::_runNumber = -1;
int StjBEMCMuDst::_eventId = -1;
StjTowerEnergyList StjBEMCMuDst::_list;

StjBEMCMuDst::StjBEMCMuDst(bool doTowerSwapFix) : _bemcTables(0)
{
  // If data, StEmcADCtoEMaker will be in the chain
  if (StEmcADCtoEMaker* adc2e = (StEmcADCtoEMaker*)StMaker::GetChain()->GetMakerInheritsFrom("StEmcADCtoEMaker")) {
    _bemcTables = adc2e->getBemcData()->getTables();
  }

  // If simulation, StEmcSimulatorMaker will be in the chain
  if (StEmcSimulatorMaker* emcSim = (StEmcSimulatorMaker*)StMaker::GetChain()->GetMakerInheritsFrom("StEmcSimulatorMaker")) {
    _bemcTables = emcSim->getTables();
  }

  assert(_bemcTables);

  _setVertex = false;
}

StjTowerEnergyList StjBEMCMuDst::getEnergyList()
{
  if(isNewEvent()) _list = getlist();
  return _list;
}

void StjBEMCMuDst::setVertex(float vx, float vy, float vz)
{
  _setVertex = true;

  _vx = vx;
  _vy = vy;
  _vz = vz;
}

bool StjBEMCMuDst::isNewEvent()
{
  if(_runNumber != StMuDst::event()->runId()) return true;
  if(_eventId != StMuDst::event()->eventId()) return true;
  return false;
}

StjTowerEnergyList StjBEMCMuDst::getlist()
{
  _runNumber = StMuDst::event()->runId();
  _eventId = StMuDst::event()->eventId();

  StjTowerEnergyList ret;

  StEmcCollection* emcCollection = findEmcCollection();
  if (emcCollection) {
    StEmcDetector* detector = emcCollection->detector(kBarrelEmcTowerId);
    if (detector) {
      for(unsigned int m = 1; m <= detector->numberOfModules(); ++m) {
	StSPtrVecEmcRawHit& rawHits = detector->module(m)->hits();
	for(size_t k = 0; k < rawHits.size(); ++k) {
	  ret.push_back(readTowerHit(*rawHits[k]));
	}
      }
    }
  }

  return ret;
}

StEmcCollection* StjBEMCMuDst::findEmcCollection()
{
  StEvent* event = dynamic_cast<StEvent*>(StMaker::GetChain()->GetInputDS("StEvent") );
  return (event) ? event->emcCollection() : StMuDst::emcCollection();
}

StjTowerEnergy StjBEMCMuDst::readTowerHit(const StEmcRawHit& hit)
{
  StjTowerEnergy ret;

  ret.runNumber = StMuDst::event()->runId();
  ret.eventId = StMuDst::event()->eventId();

  ret.detectorId = kBarrelEmcTowerId;

  int towerId;
  StEmcGeom::instance("bemc")->getId(hit.module(), hit.eta(), abs(hit.sub()), towerId);

  ret.towerId = towerId;

  float towerX, towerY, towerZ;
  StEmcGeom::instance("bemc")->getXYZ(towerId, towerX, towerY, towerZ);
  TVector3 tower(towerX, towerY, towerZ);

  ret.towerR   = tower.Perp();
  ret.towerEta = tower.Eta();
  ret.towerPhi = tower.Phi();

  if (_setVertex) {
    ret.vertexX = _vx;
    ret.vertexY = _vy;
    ret.vertexZ = _vz;
  }
  else {
    StThreeVectorF vertex = StMuDst::event()->primaryVertexPosition();

    ret.vertexX = vertex.x();
    ret.vertexY = vertex.y();
    ret.vertexZ = vertex.z(); 
  }

  ret.energy = hit.energy();
  ret.adc    = hit.adc();

  float pedestal, rms;
  int CAP(0);
  _bemcTables->getPedestal(BTOW, towerId, CAP, pedestal, rms);
  ret.pedestal = pedestal;
  ret.rms = rms;

  int status;
  _bemcTables->getStatus(BTOW, towerId, status);
  ret.status = status;

  return ret;
}
