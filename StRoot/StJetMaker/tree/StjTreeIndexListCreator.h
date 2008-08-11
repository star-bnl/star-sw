// -*- mode: c++;-*-
// $Id: StjTreeIndexListCreator.h,v 1.1 2008/08/11 00:25:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLISTCREATOR_H
#define STJTREEINDEXLISTCREATOR_H

#include <TObject.h>

#include <string>
#include <vector>
#include <set>

class TDirectory;

class StjTreeIndexList;

class StjTreeIndexListCreator : public TObject {

public:
  StjTreeIndexListCreator(TDirectory *file) { }
  virtual ~StjTreeIndexListCreator() { }

  void AddTrgTreeName(const char* treeName) { _trgTreeNameList.push_back(treeName); }

  StjTreeIndexList* create();

  typedef std::vector<std::string> TrgTreeNameList;
  TrgTreeNameList trgTreeNameList() const { return _trgTreeNameList; }

private:

  TrgTreeNameList _trgTreeNameList;

  ClassDef(StjTreeIndexListCreator, 1)

};

#endif // STJTREEINDEXLISTCREATOR_H
