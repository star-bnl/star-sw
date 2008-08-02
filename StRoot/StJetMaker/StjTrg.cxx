// $Id: StjTrg.cxx,v 1.2 2008/08/02 19:22:28 tai Exp $
#include "StjTrg.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

using namespace std;

int StjTrg::runNumber()
{
  return _uDstMaker->muDst()->event()->runId();
}

int StjTrg::eventId()
{
  return _uDstMaker->muDst()->event()->eventId();
}

bool StjTrg::hard()
{
  return _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId);
}

bool StjTrg::soft()
{
  return _soft->soft(_trgId);
}

double StjTrg::prescale()
{
  return StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];
}

double StjTrg::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

vector<int> StjTrg::towers()
{
  return _soft->towers(_trgId);
}

vector<int> StjTrg::jetPatches()
{
  return _soft->jetPatches(_trgId);
}
