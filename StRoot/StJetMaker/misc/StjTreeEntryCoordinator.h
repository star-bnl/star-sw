// -*- mode: c++;-*-
// $Id: StjTreeEntryCoordinator.h,v 1.1 2008/08/02 23:10:27 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#ifndef STJTREEENTRYCOORDINATOR_H
#define STJTREEENTRYCOORDINATOR_H

#include <Rtypes.h>

#include <string>
#include <vector>
#include <set>

class TDirectory;

class StjTreeEntryCoordinator {

public:
  StjTreeEntryCoordinator(TDirectory* file) 
    : _indexMajorName("runNumber")
    , _indexMinorName("eventId")
    , _file(file)
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

  void AddTrgTreeName(const char* treeName) { _trgTreeNameList.push_back(treeName); }

  typedef std::vector<std::string> TrgTreeNameList;
  TrgTreeNameList trgTreeNameList() const { return _trgTreeNameList; }

  TDirectory* file() { return _file; }

private:

  struct index_t {
    Int_t major;
    Int_t minor;
  };

  friend bool operator<(const index_t& v1, const index_t& v2) {
    if(v1.major != v2.major) return v1.major < v2.major;
    return v1.minor < v2.minor;
  }

  typedef std::vector<index_t> IndexList;
  typedef std::set<index_t> IndexSet;

  IndexList buildIndexListToRun();
  IndexList getIndexListOfRunsPassedFor(const char* treeName);

  std::string _indexMajorName;
  std::string _indexMinorName;

  Int_t _indexMajor;
  Int_t _indexMinor;

  IndexList _indexList;

  size_t _currentIndexOfIndexList;

  TDirectory *_file;

  bool _eof;

  TrgTreeNameList _trgTreeNameList;

};

#endif // STJTREEENTRYCOORDINATOR_H
