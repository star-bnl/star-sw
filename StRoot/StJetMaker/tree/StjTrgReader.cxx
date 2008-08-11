// $Id: StjTrgReader.cxx,v 1.1 2008/08/11 06:07:53 tai Exp $
// Copyright (C) 2008 Tai Sakuma <sakuma@bnl.gov>
#include "StjTrgReader.h"

#include <TTree.h>

ClassImp(StjTrgReader)

void StjTrgReader::SetBranchAddress(TTree *tree)
{
  tree->SetBranchAddress("runNumber"     , &_runNumber       );
  tree->SetBranchAddress("eventId"       , &_eventId         );
  tree->SetBranchAddress("vertexZ"       , &_vertexZ         );
  tree->SetBranchAddress("trigID"        , &_trigID          );
  tree->SetBranchAddress("prescale"      , &_prescale        );
  tree->SetBranchAddress("passed"        , &_passed          );
  tree->SetBranchAddress("hard"          , &_hard            );
  tree->SetBranchAddress("soft"          , &_soft            );
  if(tree->GetBranch("nTowers")) {
    tree->SetBranchAddress("nTowers"       , &_nTowers         );
    tree->SetBranchAddress("towerId"       ,  _towerId         );
  }
  if(tree->GetBranch("nJetPatches")) {
    tree->SetBranchAddress("nJetPatches"   , &_nJetPatches     );
    tree->SetBranchAddress("jetPatchId"    ,  _jetPatchId      );
  }
}

void StjTrgReader::clearEntry()
{
  __id        = 0;
  __runNumber = 0;
  __eventId   = 0;
  __hard      = false;
  __soft      = false;
  __pass      = false;
  __prescale  = 0;
  __vertexZ   = 0;
  __towers.clear();
  __jetPatches.clear();

  _nTowers = 0;
  _nJetPatches = 0;
}

void StjTrgReader::readEntry()
{
  __id        = _trigID;
  __runNumber = _runNumber;
  __eventId   = _eventId;
  __hard      = _hard;
  __soft      = _soft;
  __pass      = _passed;
  __prescale  = _prescale;
  __vertexZ   = _vertexZ;

  for(int i = 0; i != _nTowers; ++i) {
    __towers.push_back(_towerId[i]);
  }

  for(int i = 0; i != _nJetPatches; ++i) {
    __jetPatches.push_back(_jetPatchId[i]);
  }
}


