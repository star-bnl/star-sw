// $Id: StjTreeReaderTwoTrees.cxx,v 1.1 2008/09/15 05:50:07 tai Exp $
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
