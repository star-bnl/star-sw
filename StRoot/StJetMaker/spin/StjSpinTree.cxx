// $Id: StjSpinTree.cxx,v 1.1 2008/11/05 05:48:26 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjSpinTree.h"

#include "StjSpinReader.h"

ClassImp(StjSpinTree)

int StjSpinTree::runNumber()
{
  return _reader->runNumber();
}
 
int StjSpinTree::eventId()
{
  return _reader->eventId();
}

int StjSpinTree::bx7()
{
  return _reader->bx7();
}

int StjSpinTree::bx48()
{
  return _reader->bx48();
}

int StjSpinTree::spin4()
{
  return _reader->spin4();
}

int StjSpinTree::bbcTimebin()
{
  return _reader->bbcTimebin();
}

double StjSpinTree::vertexZ()
{
  return _reader->vertexZ();
}

