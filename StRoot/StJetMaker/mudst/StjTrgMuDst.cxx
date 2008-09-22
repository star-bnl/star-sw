// $Id: StjTrgMuDst.cxx,v 1.6 2008/09/22 00:06:49 tai Exp $
#include "StjTrgMuDst.h"

#include "StjTrgSoft.h"
#include "StjTrgPassCondition.h"
#include "StjTrgBEMCJetPatchTowerIdMap.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

ClassImp(StjTrgMuDst)

using namespace std;

StjTrgMuDst::StjTrgMuDst(int trgId, StjTrgPassCondition* passCondition, StMuDstMaker* uDstMaker, StjTrgSoft* soft)
  : _trgId(trgId)
  , _passCondition(passCondition)
  , _soft(soft)
  , _uDstMaker(uDstMaker)
{ 
  _soft->setTrg(this);
}

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
  return _soft->soft();
}

bool StjTrgMuDst::passed() const
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
  return _soft->towers();
}

vector<int> StjTrgMuDst::towerDsmAdc()
{
  return _soft->towerDsmAdc();
}

vector<unsigned int> StjTrgMuDst::towerAdc()
{
  return _soft->towerAdc();
}

vector<double> StjTrgMuDst::towerEnergy()
{
  return _soft->towerEnergy();
}

vector<double> StjTrgMuDst::towerEt()
{
  return _soft->towerEt();
}

vector<int> StjTrgMuDst::jetPatches()
{
  return _soft->jetPatches();
}

vector<int> StjTrgMuDst::jetPatchDsmAdc()
{
  return _soft->jetPatchDsmAdc();
}

vector<unsigned int> StjTrgMuDst::jetPatchAdc()
{
  return _soft->jetPatchAdc();
}

vector<double> StjTrgMuDst::jetPatchEnergy()
{
  return _soft->jetPatchEnergy();
}

vector<double> StjTrgMuDst::jetPatchEt()
{
  return _soft->jetPatchEt();
}
