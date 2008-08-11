// $Id: StjTrgTree.cxx,v 1.2 2008/08/11 06:07:58 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgTree.h"

#include "StjTrgReader.h"

ClassImp(StjTrgTree)

using namespace std;

int StjTrgTree::id()
{
  return _reader->id();
}

int StjTrgTree::runNumber()
{
  return _reader->runNumber();
}
 
int StjTrgTree::eventId()
{
  return _reader->eventId();
}

bool StjTrgTree::hard() const
{
  return _reader->hard();
}

bool StjTrgTree::soft() const
{
  return _reader->soft();
}

bool StjTrgTree::pass()
{
  return _reader->pass();
}

double StjTrgTree::prescale()
{
  return _reader->prescale();
}

double StjTrgTree::vertexZ()
{
  return _reader->vertexZ();
}

vector<int> StjTrgTree::towers()
{
  return _reader->towers();
}

vector<int> StjTrgTree::jetPatches()
{
  return _reader->jetPatches();
}
