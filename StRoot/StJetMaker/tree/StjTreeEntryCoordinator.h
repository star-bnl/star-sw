// -*- mode: c++;-*-
// $Id: StjTreeEntryCoordinator.h,v 1.2 2008/08/11 02:22:20 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEENTRYCOORDINATOR_H
#define STJTREEENTRYCOORDINATOR_H

#include "StjTreeIndexList.h"

#include <TObject.h>

#include <Rtypes.h>

#include <string>
#include <vector>
#include <set>

class TDirectory;

class StjTreeEntryCoordinator : public TObject {

public:
  StjTreeEntryCoordinator(const StjTreeIndexList& idxList) 
    : _indexMajorName("runNumber")
    , _indexMinorName("eventId")
    , _indexList(idxList)
    , _eof(false)
  { }

  virtual ~StjTreeEntryCoordinator() { }

  void Init();
  void Make();

  bool eof() const { return _eof; }

  const char* indexMajorName() const { return _indexMajorName.c_str(); };
  const char* indexMinorName() const { return _indexMinorName.c_str(); };

  const Int_t& indexMajor() const { return _indexMajor; }
  const Int_t& indexMinor() const { return _indexMinor; }

private:

  std::string _indexMajorName;
  std::string _indexMinorName;

  Int_t _indexMajor;
  Int_t _indexMinor;

  StjTreeIndexList _indexList;

  size_t _currentIndexOfIndexList;

  bool _eof;

  ClassDef(StjTreeEntryCoordinator, 1)
};

#endif // STJTREEENTRYCOORDINATOR_H
