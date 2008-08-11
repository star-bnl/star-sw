// $Id: StjTreeEntryCoordinator.cxx,v 1.3 2008/08/11 03:51:00 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeEntryCoordinator.h"

#include "StjTreeReader.h"

#include <iostream>

ClassImp(StjTreeEntryCoordinator)

using namespace std;

void StjTreeEntryCoordinator::Init()
{
  for(ReaderList::iterator it = _readerList.begin(); it != _readerList.end(); ++it) {
    (*it)->Init();
  }

  _currentIndexOfIndexList = 0;
  _eof = _indexList.empty();
}

void StjTreeEntryCoordinator::Make()
{
  _indexMajor = _indexList[_currentIndexOfIndexList].major();
  _indexMinor = _indexList[_currentIndexOfIndexList].minor();
  ++_currentIndexOfIndexList;
  if(_indexList.size() == _currentIndexOfIndexList) _eof = true;

  for(ReaderList::iterator it = _readerList.begin(); it != _readerList.end(); ++it) {
    (*it)->GetEntryWithIndex(_indexMajor, _indexMinor);
  }

}
