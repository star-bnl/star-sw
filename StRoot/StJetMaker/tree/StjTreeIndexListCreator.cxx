// $Id: StjTreeIndexListCreator.cxx,v 1.5 2009/12/03 09:57:36 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeIndexListCreator.h"

#include "StjTreeIndexList.h"

#include <TTree.h>
#include <TDirectory.h>

#include <set>

ClassImp(StjTreeIndexListCreator)

using namespace std;

StjTreeIndexList StjTreeIndexListCreator::create()
{
  std::set<StjTreeIndex> indexSet;
  for(TrgTreeNameList::const_iterator trgName = _trgTreeNameList.begin(); trgName != _trgTreeNameList.end(); ++trgName) {
    StjTreeIndexList aList = getIndexListOfRunsPassedFor((*trgName).c_str());
    indexSet.insert(aList.begin(), aList.end());
  }
  
  StjTreeIndexList ret;;
  copy(indexSet.begin(), indexSet.end(), back_inserter(ret));
  return ret;
}

StjTreeIndexList StjTreeIndexListCreator::getIndexListOfRunsPassedFor(const char* treeName)
{
  StjTreeIndexList ret;
  TTree *tree = dynamic_cast<TTree*>(_file->Get(treeName));
  Int_t indexMajor, indexMinor;
  tree->SetBranchAddress(_indexMajorName.c_str(), &indexMajor);
  tree->SetBranchAddress(_indexMinorName.c_str(), &indexMinor);
  // Int_t passed;
  // tree->SetBranchAddress("passed", &passed);
  for(Long64_t i = 0; i < tree->GetEntries(); ++i) {
    if(tree->GetEntry(i) <= 0) continue;
    // if(!passed) continue;
    ret.push_back(StjTreeIndex(indexMajor, indexMinor));
  }
  return ret;
}
