// -*- mode: c++;-*-
// $Id: StjTreeIndexList.h,v 1.2 2008/08/11 00:53:13 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLIST_H
#define STJTREEINDEXLIST_H

#include "StjTreeIndex.h"

#include <TObject.h>

#include <ostream>
#include <string>

class StjTreeIndexList : public TObject {

public:
  StjTreeIndexList(const char* indexMajorName, const char* indexMinorName)
    : _indexMajorName(indexMajorName), _indexMinorName(indexMinorName) { }
  virtual ~StjTreeIndexList() { }

  std::string indexMajorName()  const { return _indexMajorName; }
  std::string indexMinorName()  const { return _indexMinorName; }

  friend bool operator==(const StjTreeIndexList& v1, const StjTreeIndexList& v2) {
    if( v1.indexMajorName() != v2.indexMajorName() ) return false;
    if( v1.indexMinorName() != v2.indexMinorName() ) return false;
    if( v1.size() != v2.size() ) return false;
    return true;
  }

  friend std::ostream& operator<<(std::ostream& out, const StjTreeIndexList& v)
  {
    out << "StjTreeIndexList: ";
    return out;
  }

  typedef std::vector<StjTreeIndex> IndexList;
  void push_back(const StjTreeIndex& v) { _itsList.push_back(v); }

  size_t size() const { return _itsList.size(); } 

private:

  IndexList _itsList;

  std::string _indexMajorName;
  std::string _indexMinorName;

  ClassDef(StjTreeIndexList, 1)

};

#endif // STJTREEINDEXLIST_H
