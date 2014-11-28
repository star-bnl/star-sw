// $Id: StjVertexMuDst.cxx,v 1.1 2008/08/13 19:37:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjVertexMuDst.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

ClassImp(StjVertexMuDst)

int StjVertexMuDst::runNumber()
{
  return _uDstMaker->muDst()->event()->runId();
}

int StjVertexMuDst::eventId()
{
  return _uDstMaker->muDst()->event()->eventId();
}

double StjVertexMuDst::vertexZ()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().z();
}

double StjVertexMuDst::vertexY()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().y();
}

double StjVertexMuDst::vertexX()
{
  return _uDstMaker->muDst()->event()->primaryVertexPosition().x();
}
