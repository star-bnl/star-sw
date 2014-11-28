// -*- mode: c++;-*-
// $Id: StjTreeEntryCoordinator.h,v 1.4 2008/08/11 04:32:19 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEENTRYCOORDINATOR_H
#define STJTREEENTRYCOORDINATOR_H

#include "StjTreeIndexList.h"

#include <TObject.h>

#include <vector>

class StjTreeReader;

class StjTreeEntryCoordinator : public TObject {

public:
  StjTreeEntryCoordinator(const StjTreeIndexList& idxList) 
    : _indexList(idxList)
  { }

  virtual ~StjTreeEntryCoordinator() { }

  void Init();
  void Make();

  bool eof() const { return (_it == _indexList.end()); }

  void AddReader(StjTreeReader* reader) { _readerList.push_back(reader); }

private:

  typedef std::vector<StjTreeReader*> ReaderList;
  ReaderList _readerList;

  StjTreeIndexList _indexList;

  StjTreeIndexList::iterator _it;

  ClassDef(StjTreeEntryCoordinator, 1)
};

#endif // STJTREEENTRYCOORDINATOR_H
