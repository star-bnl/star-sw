// $Id: StjTreeEntryMaker.cxx,v 1.1 2008/08/02 04:05:57 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>

#include "StjTreeEntryMaker.h"

#include "StjTreeEntryCoordinator.h"

#include <TFile.h>

ClassImp(StJetTreeEntryMaker)

StJetTreeEntryMaker::StJetTreeEntryMaker(const Char_t *name, TDirectory* file)
  : _file(file)
  , _coord(new StJetTreeEntryCoordinator(_file))
{

}

StJetTreeEntryMaker::StJetTreeEntryMaker(const Char_t *name, const char* inputFileName)
  : _file(new TFile(inputFileName))
  , _coord(new StJetTreeEntryCoordinator(_file))
{

}

void StJetTreeEntryMaker::AddTrgTreeName(const char* treeName)
{
  _coord->AddTrgTreeName(treeName);
}

Int_t StJetTreeEntryMaker::Init()
{
  _coord->Init();
  return kStOk;
}

Int_t StJetTreeEntryMaker::Make()
{
  _coord->Make();
  if(_coord->eof()) return kStEOF;
  return kStOk;
}



