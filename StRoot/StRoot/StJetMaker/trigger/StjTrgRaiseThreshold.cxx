// $Id: StjTrgRaiseThreshold.cxx,v 1.1 2008/08/20 16:24:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgRaiseThreshold.h"

ClassImp(StjTrgRaiseThreshold)

using namespace std;

void StjTrgRaiseThreshold::readIfNewEvent() const
{
  if(isNewEvent()) readNewEvent();
}

bool StjTrgRaiseThreshold::isNewEvent() const
{
  if(_runNumber != _src->runNumber()) return true;
  if(_eventId != _src->eventId()) return true;
  return false;
}

void StjTrgRaiseThreshold::readNewEvent() const
{
  _runNumber = _src->runNumber();
  _eventId = _src->eventId();

  read();
}


