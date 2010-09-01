// $Id: StjTreeReader.cxx,v 1.1 2008/08/11 04:48:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTreeReader.h"

#include "StjTreeIndex.h"

#include <TTree.h>

ClassImp(StjTreeReader)

void StjTreeReader::Init()
{
  SetBranchAddress(_tree);
}

Long64_t StjTreeReader::GetEntryWithIndex(const StjTreeIndex& idx)
{
  GetEntryWithIndex(idx.major(), idx.minor());
}

Long64_t StjTreeReader::GetEntryWithIndex(Int_t major, Int_t minor)
{
  clearEntry();

  Long64_t ret = _tree->GetEntryWithIndex(major, minor);

  if(ret <= 0) return ret;

  readEntry();

  return ret;
}
