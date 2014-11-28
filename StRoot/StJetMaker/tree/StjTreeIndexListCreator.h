// -*- mode: c++;-*-
// $Id: StjTreeIndexListCreator.h,v 1.2 2008/08/11 01:28:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLISTCREATOR_H
#define STJTREEINDEXLISTCREATOR_H

#include "StjTreeIndexList.h"

#include <TObject.h>

#include <string>
#include <vector>

class TDirectory;


class StjTreeIndexListCreator : public TObject {

public:
  StjTreeIndexListCreator(TDirectory *file) 
    : _indexMajorName("runNumber"), _indexMinorName("eventId")
    , _file(file)
  { }
  virtual ~StjTreeIndexListCreator() { }

  void AddTrgTreeName(const char* treeName) { _trgTreeNameList.push_back(treeName); }

  StjTreeIndexList create();

  typedef std::vector<std::string> TrgTreeNameList;
  TrgTreeNameList trgTreeNameList() const { return _trgTreeNameList; }

private:

  StjTreeIndexList getIndexListOfRunsPassedFor(const char* treeName);

  std::string _indexMajorName;
  std::string _indexMinorName;

  TDirectory *_file;

  TrgTreeNameList _trgTreeNameList;

  ClassDef(StjTreeIndexListCreator, 1)

};

#endif // STJTREEINDEXLISTCREATOR_H
