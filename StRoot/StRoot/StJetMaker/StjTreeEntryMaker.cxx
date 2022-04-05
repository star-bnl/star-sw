// $Id: StjTreeEntryMaker.cxx,v 1.3 2008/08/10 23:04:36 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "StjTreeEntryMaker.h"

#include "StjTreeEntryCoordinator.h"

#include <TFile.h>

ClassImp(StjTreeEntryMaker)

Int_t StjTreeEntryMaker::Init()
{
  _coord->Init();
  return kStOk;
}

Int_t StjTreeEntryMaker::Make()
{
  _coord->Make();
  if(_coord->eof()) return kStEOF;
  return kStOk;
}



