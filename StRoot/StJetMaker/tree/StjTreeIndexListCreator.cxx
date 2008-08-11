// $Id: StjTreeIndexListCreator.cxx,v 1.1 2008/08/11 00:25:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeIndexListCreator.h"

#include "StjTreeIndexList.h"

ClassImp(StjTreeIndexListCreator)

using namespace std;

StjTreeIndexList* StjTreeIndexListCreator::create()
{
  StjTreeIndexList* ret = new StjTreeIndexList("runNumber", "eventId");
  return ret;
}
