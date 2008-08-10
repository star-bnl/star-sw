// -*- mode: c++;-*-
// $Id: StjTreeIndex.h,v 1.1 2008/08/10 23:41:45 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEX_H
#define STJTREEINDEX_H

#include <TObject.h>

class StjTreeIndex : public TObject {

public:
  StjTreeIndex() { }
  virtual ~StjTreeIndex() { }

  friend bool operator<(const StjTreeIndex& v1, const StjTreeIndex& v2) {
    if(v1.major != v2.major) return v1.major < v2.major;
    return v1.minor < v2.minor;
  }

private:

  ClassDef(StjTreeIndex, 1)

};

#endif // STJTREEINDEX_H
