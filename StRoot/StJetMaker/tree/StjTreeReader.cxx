// $Id: StjTreeReader.cxx,v 1.4 2015/08/14 16:38:28 rfatemi Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeReader.h"

#include "StjTreeIndex.h"

#include <TTree.h>

ClassImp(StjTreeReader)

void StjTreeReader::Init()
{
  _tree->BuildIndex("runNumber", "eventId");
  SetBranchAddress(_tree);
}

Long64_t StjTreeReader::GetEntryWithIndex(const StjTreeIndex& idx)
{
  return GetEntryWithIndex(idx.big(), idx.small());
}

Long64_t StjTreeReader::GetEntryWithIndex(Int_t big, Int_t small)
{
  clearEntry();

  Long64_t ret = _tree->GetEntryWithIndex(big, small);

  if(ret <= 0) return ret;

  readEntry();

  return ret;
}
