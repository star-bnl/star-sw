// $Id: StjTreeReader.cxx,v 1.3 2009/12/03 09:57:36 pibero Exp $
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
  return GetEntryWithIndex(idx.major(), idx.minor());
}

Long64_t StjTreeReader::GetEntryWithIndex(Int_t major, Int_t minor)
{
  clearEntry();

  Long64_t ret = _tree->GetEntryWithIndex(major, minor);

  if(ret <= 0) return ret;

  readEntry();

  return ret;
}
