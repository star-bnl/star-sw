// $Id: StjVertexReader.cxx,v 1.1 2008/08/13 19:37:29 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjVertexReader.h"

#include <TTree.h>

ClassImp(StjVertexReader)

void StjVertexReader::SetBranchAddress(TTree *tree)
{
  tree->SetBranchAddress("runNumber"     , &_runNumber       );
  tree->SetBranchAddress("eventId"       , &_eventId         );
  tree->SetBranchAddress("vertexZ"       , &_vertexZ         );
  tree->SetBranchAddress("vertexY"       , &_vertexY         );
  tree->SetBranchAddress("vertexX"       , &_vertexX         );
}

void StjVertexReader::clearEntry()
{
  __runNumber = 0;
  __eventId   = 0;
  __vertexZ   = 0;
  __vertexY   = 0;
  __vertexX   = 0;
}

void StjVertexReader::readEntry()
{
  __runNumber = _runNumber;
  __eventId   = _eventId;
  __vertexZ   = _vertexZ;
  __vertexY   = _vertexY;
  __vertexX   = _vertexX;
}
