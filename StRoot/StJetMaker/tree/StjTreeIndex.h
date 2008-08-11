// -*- mode: c++;-*-
// $Id: StjTreeIndex.h,v 1.3 2008/08/11 00:53:13 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEINDEX_H
#define STJTREEINDEX_H

#include <Rtypes.h>
#include <TObject.h>

class StjTreeIndex : public TObject {

public:
  StjTreeIndex(Int_t major = 0, Int_t minor = 0) : _major(major), _minor(minor) { }
  virtual ~StjTreeIndex() { }

  Int_t major() const { return _major; }
  Int_t minor() const { return _minor; }

private:

  Int_t _major;
  Int_t _minor;

  ClassDef(StjTreeIndex, 1)

};

#endif // STJTREEINDEX_H
