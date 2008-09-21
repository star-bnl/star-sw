// $Id: StjTrgDisableTower.cxx,v 1.1 2008/09/21 19:11:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgDisableTower.h"

ClassImp(StjTrgDisableTower)

using namespace std;

void StjTrgDisableTower::readIfNewEvent() const
{
  if(isNewEvent()) readNewEvent();
}

bool StjTrgDisableTower::isNewEvent() const
{
  if(_runNumber != _src->runNumber()) return true;
  if(_eventId != _src->eventId()) return true;
  return false;
}

void StjTrgDisableTower::readNewEvent() const
{
  _runNumber = _src->runNumber();
  _eventId = _src->eventId();

  read();
}


