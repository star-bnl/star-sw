// $Id: StJetTrg.cxx,v 1.2 2008/07/24 02:14:48 tai Exp $
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

bool StJetTrg::hard()
{
  return _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId);
}

bool StJetTrg::soft()
{
  return _soft->soft(_trgId);
}

double StJetTrg::prescale()
{
  return StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];
}

double StJetTrg::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

vector<int> StJetTrg::towers()
{
  return _soft->towers(_trgId);
}

vector<int> StJetTrg::jetPatches()
{
  return _soft->jetPatches(_trgId);
}
