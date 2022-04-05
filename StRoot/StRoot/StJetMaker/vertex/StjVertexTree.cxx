// $Id: StjVertexTree.cxx,v 1.1 2008/08/13 19:37:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjVertexTree.h"

#include "StjVertexReader.h"

ClassImp(StjVertexTree)

int StjVertexTree::runNumber()
{
  return _reader->runNumber();
}
 
int StjVertexTree::eventId()
{
  return _reader->eventId();
}

double StjVertexTree::vertexZ()
{
  return _reader->vertexZ();
}

double StjVertexTree::vertexY()
{
  return _reader->vertexY();
}

double StjVertexTree::vertexX()
{
  return _reader->vertexX();
}
