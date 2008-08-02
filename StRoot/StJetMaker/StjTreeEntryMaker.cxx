// $Id: StjTreeEntryMaker.cxx,v 1.2 2008/08/02 19:22:28 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "StjTreeEntryMaker.h"

#include "StjTreeEntryCoordinator.h"

#include <TFile.h>

ClassImp(StjTreeEntryMaker)

StjTreeEntryMaker::StjTreeEntryMaker(const Char_t *name, TDirectory* file)
  : _file(file)
  , _coord(new StjTreeEntryCoordinator(_file))
{

}

StjTreeEntryMaker::StjTreeEntryMaker(const Char_t *name, const char* inputFileName)
  : _file(new TFile(inputFileName))
  , _coord(new StjTreeEntryCoordinator(_file))
{

}

void StjTreeEntryMaker::AddTrgTreeName(const char* treeName)
{
  _coord->AddTrgTreeName(treeName);
}

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



