// $Id: StjTrgMuDst.cxx,v 1.2 2008/08/08 22:53:18 tai Exp $
#include "StjTrgMuDst.h"

#include "StjTrgSoftware.h"
#include "StjTrgPassCondition.h"

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

vector<int> StjTrgMuDst::jetPatches()
{
  return _soft->jetPatches(_trgId);
}
