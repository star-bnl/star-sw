// -*- mode: c++;-*-
// $Id: StjTreeIndexList.h,v 1.1 2008/08/11 00:25:14 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEXLIST_H
#define STJTREEINDEXLIST_H

#include <TObject.h>

#include <ostream>
#include <string>

class StjTreeIndexList : public TObject {

public:
  StjTreeIndexList(const char* indexMajorName, const char* indexMinorName)
    : _indexMajorName(indexMajorName), _indexMinorName(indexMinorName) { }
  virtual ~StjTreeIndexList() { }

  friend bool operator==(const StjTreeIndexList& v1, const StjTreeIndexList& v2) {
    if( v1._indexMajorName != v2._indexMajorName ) return false;
    if( v1._indexMinorName != v2._indexMinorName ) return false;
    return true;
  }

  friend std::ostream& operator<<(std::ostream& out, const StjTreeIndexList& v)
  {
    out << "StjTreeIndexList: ";
    return out;
  }

  class index_t {
  public:
    index_t(int Ma, int Mi) : _major(Ma), _minor(Mi) { }
    virtual ~index_t() { }
    Int_t _major;
    Int_t _minor;
  };

private:

  std::string _indexMajorName;
  std::string _indexMinorName;

  ClassDef(StjTreeIndexList, 1)

};

#endif // STJTREEINDEXLIST_H
