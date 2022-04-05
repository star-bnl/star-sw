// $Id: StjTreeEntryCoordinator.cxx,v 1.4 2008/08/11 04:32:19 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeEntryCoordinator.h"

#include "StjTreeReader.h"

#include <iostream>

ClassImp(StjTreeEntryCoordinator)

using namespace std;

void StjTreeEntryCoordinator::Init()
{
  for(ReaderList::iterator reader = _readerList.begin(); reader != _readerList.end(); ++reader) {
    (*reader)->Init();
  }

  _it = _indexList.begin();
}

void StjTreeEntryCoordinator::Make()
{
  if(eof()) return;

  for(ReaderList::iterator reader = _readerList.begin(); reader != _readerList.end(); ++reader) {
    (*reader)->GetEntryWithIndex(*_it);
  }

  ++_it;
}
