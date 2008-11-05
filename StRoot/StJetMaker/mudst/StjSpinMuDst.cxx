// $Id: StjSpinMuDst.cxx,v 1.1 2008/11/05 05:48:19 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjSpinMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

ClassImp(StjSpinMuDst)

int StjSpinMuDst::runNumber()
{
  return _uDstMaker->muDst()->event()->runId();
}

int StjSpinMuDst::eventId()
{
  return _uDstMaker->muDst()->event()->eventId();
}

int StjSpinMuDst::bx7()
{
  return _uDstMaker->muDst()->event()->l0Trigger().bunchCrossingId7bit( runNumber() );
}

int StjSpinMuDst::bx48()
{
  return _uDstMaker->muDst()->event()->l0Trigger().bunchCrossingId();
}

int StjSpinMuDst::spin4()
{
  return _uDstMaker->muDst()->event()->l0Trigger().spinBits( runNumber() );
}

int StjSpinMuDst::bbcTimebin()
{
  return _uDstMaker->muDst()->event()->bbcTriggerDetector().onlineTimeDifference()/32;
}

double StjSpinMuDst::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

