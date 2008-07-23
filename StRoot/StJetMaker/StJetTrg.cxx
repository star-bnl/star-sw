// $Id: StJetTrg.cxx,v 1.1 2008/07/23 20:25:41 tai Exp $
#include "StJetTrg.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

using namespace std;

int StJetTrg::runNumber()
{
  return _uDstMaker->muDst()->event()->runId();
}

int StJetTrg::eventId()
{
  return _uDstMaker->muDst()->event()->eventId();
}

bool StJetTrg::hard(int trgId)
{
  return _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trgId);
}

bool StJetTrg::soft(int trgId)
{
  return _soft->soft(trgId);
}

double StJetTrg::prescale(int trgId)
{
  return StDetectorDbTriggerID::instance()->getTotalPrescales()[trgId];
}

double StJetTrg::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

vector<int> StJetTrg::towers(int trgId)
{
  return _soft->towers(trgId);
}

vector<int> StJetTrg::jetPatches(int trgId)
{
  return _soft->jetPatches(trgId);
}
