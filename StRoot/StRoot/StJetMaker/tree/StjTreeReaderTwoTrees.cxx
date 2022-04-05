// $Id: StjTreeReaderTwoTrees.cxx,v 1.2 2009/12/03 09:57:36 pibero Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeReaderTwoTrees.h"

#include "StjTreeIndex.h"

#include <TTree.h>

#include <iostream>

ClassImp(StjTreeReaderTwoTrees)

using namespace std;

void StjTreeReaderTwoTrees::Init()
{
  _tree1->BuildIndex("runNumber", "eventId");
  _tree2->BuildIndex("runNumber", "eventId");
  SetBranchAddress(_tree1, _tree2);
}

Long64_t StjTreeReaderTwoTrees::GetEntryWithIndex(const StjTreeIndex& idx)
{
  return StjTreeReader::GetEntryWithIndex(idx);
}

Long64_t StjTreeReaderTwoTrees::GetEntryWithIndex(Int_t major, Int_t minor)
{
  clearEntry();

  Long64_t ret1 = _tree1->GetEntryWithIndex(major, minor);

  if(ret1 <= 0) return ret1;

  Long64_t ret2 = _tree2->GetEntryWithIndex(major, minor);

  if(ret2 <= 0) return ret2;

  readEntry();

  return ret1 + ret2;
}
