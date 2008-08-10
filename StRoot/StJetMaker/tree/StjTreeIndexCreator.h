// -*- mode: c++;-*-
// $Id: StjTreeIndexCreator.h,v 1.1 2008/08/10 23:41:46 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXCREATOR_H
#define STJTREEINDEXCREATOR_H

#include "StjTreeIndex.h"

#include <TObject.h>

#include <string>
#include <vector>
#include <set>

class TDirectory;

class StjTreeIndexCreator : public TObject {

public:
  StjTreeIndexCreator(TDirectory *file) { }
  virtual ~StjTreeIndexCreator() { }

  void AddTrgTreeName(const char* treeName) { _trgTreeNameList.push_back(treeName); }

  StjTreeIndex create() { }

  typedef std::vector<std::string> TrgTreeNameList;
  TrgTreeNameList trgTreeNameList() const { return _trgTreeNameList; }

private:

  TrgTreeNameList _trgTreeNameList;

  ClassDef(StjTreeIndexCreator, 1)

};

#endif // STJTREEINDEXCREATOR_H
