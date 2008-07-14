// $Id: StJetTreeEntryCoordinator.cxx,v 1.1 2008/07/14 03:35:41 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StJetTreeEntryCoordinator.h"

#include <TDirectory.h>
#include <TTree.h>

#include <iostream>

using namespace std;

void StJetTreeEntryCoordinator::Init()
{
  _indexList = buildIndexListToRun();
  _currentIndexOfIndexList = 0;
  _eof = _indexList.empty();
}

void StJetTreeEntryCoordinator::Make()
{
  _indexMajor = _indexList[_currentIndexOfIndexList].major;
  _indexMinor = _indexList[_currentIndexOfIndexList].minor;
  ++_currentIndexOfIndexList;
  if(_indexList.size() == _currentIndexOfIndexList) _eof = true;
}

StJetTreeEntryCoordinator::IndexList StJetTreeEntryCoordinator::buildIndexListToRun()
{
  IndexSet indexSet;
  for(TrgTreeNameList::const_iterator trgName = _trgTreeNameList.begin(); trgName != _trgTreeNameList.end(); ++trgName) {
    IndexList aList = getIndexListOfRunsPassedFor((*trgName).c_str());
    indexSet.insert(aList.begin(), aList.end());
  }
  
  IndexList ret;
  copy(indexSet.begin(), indexSet.end(), back_inserter(ret));
  return ret;
}

StJetTreeEntryCoordinator::IndexList StJetTreeEntryCoordinator::getIndexListOfRunsPassedFor(const char* treeName)
{
  IndexList ret;
  TTree *tree = dynamic_cast<TTree*>(_file->Get(treeName));
  Int_t indexMajor, indexMinor, passed;
  tree->SetBranchAddress(_indexMajorName.c_str(), &indexMajor);
  tree->SetBranchAddress(_indexMinorName.c_str(), &indexMinor);
  tree->SetBranchAddress("passed", &passed);
  for(Long64_t i = 0; i < tree->GetEntries(); ++i) {
    if(tree->GetEntry(i) <= 0) continue;
    if(!passed) continue;
    index_t index;
    index.major = indexMajor;
    index.minor = indexMinor;
    ret.push_back(index);
  }
  return ret;
}
