// $Id: StjTreeEntryCoordinator.cxx,v 1.2 2008/08/11 02:22:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeEntryCoordinator.h"

#include <iostream>

ClassImp(StjTreeEntryCoordinator)

using namespace std;

void StjTreeEntryCoordinator::Init()
{
  _currentIndexOfIndexList = 0;
  _eof = _indexList.empty();
}

void StjTreeEntryCoordinator::Make()
{
  _indexMajor = _indexList[_currentIndexOfIndexList].major();
  _indexMinor = _indexList[_currentIndexOfIndexList].minor();
  ++_currentIndexOfIndexList;
  if(_indexList.size() == _currentIndexOfIndexList) _eof = true;
}
