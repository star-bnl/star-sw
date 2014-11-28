// $Id: StjTrgTree.cxx,v 1.6 2008/09/21 19:11:47 tai Exp $
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

bool StjTrgTree::passed() const
{
  return _reader->passed();
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

vector<int> StjTrgTree::towerDsmAdc()
{
  return _reader->towerDsmAdc();
}

vector<unsigned int> StjTrgTree::towerAdc()
{
  return _reader->towerAdc();
}

vector<double> StjTrgTree::towerEnergy()
{
  return _reader->towerEnergy();
}

vector<double> StjTrgTree::towerEt()
{
  return _reader->towerEt();
}

vector<int> StjTrgTree::jetPatches()
{
  return _reader->jetPatches();
}

vector<int> StjTrgTree::jetPatchDsmAdc()
{
  return _reader->jetPatchDsmAdc();
}

vector<unsigned int> StjTrgTree::jetPatchAdc()
{
  return _reader->jetPatchAdc();
}

vector<double> StjTrgTree::jetPatchEnergy()
{
  return _reader->jetPatchEnergy();
}

vector<double> StjTrgTree::jetPatchEt()
{
  return _reader->jetPatchEt();
}
