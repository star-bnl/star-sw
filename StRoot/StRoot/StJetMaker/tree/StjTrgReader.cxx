// $Id: StjTrgReader.cxx,v 1.5 2008/08/22 16:12:40 tai Exp $
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
    tree->SetBranchAddress("towerDsmAdc"   ,  _towerDsmAdc     );
    tree->SetBranchAddress("towerAdc"      ,  _towerAdc        );
    tree->SetBranchAddress("towerEnergy"   ,  _towerEnergy     );
    tree->SetBranchAddress("towerEt"       ,  _towerEt         );
  }
  if(tree->GetBranch("nJetPatches")) {
    tree->SetBranchAddress("nJetPatches"   , &_nJetPatches     );
    tree->SetBranchAddress("jetPatchId"    ,  _jetPatchId      );
    tree->SetBranchAddress("jetPatchDsmAdc",  _jetPatchDsmAdc  );
    tree->SetBranchAddress("jetPatchAdc"   ,  _jetPatchAdc     );
    tree->SetBranchAddress("jetPatchEnergy",  _jetPatchEnergy  );
    tree->SetBranchAddress("jetPatchEt"    ,  _jetPatchEt      );
  }
}

void StjTrgReader::clearEntry()
{
  __id        = 0;
  __runNumber = 0;
  __eventId   = 0;
  __hard      = false;
  __soft      = false;
  __passed    = false;
  __prescale  = 0;
  __vertexZ   = 0;

  __towers.clear();
  __towerDsmAdc.clear();
  __towerAdc.clear();
  __towerEnergy.clear();
  __towerEt.clear();

  __jetPatches.clear();
  __jetPatchDsmAdc.clear();
  __jetPatchAdc.clear();
  __jetPatchEnergy.clear();
  __jetPatchEt.clear();

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
  __passed    = _passed;
  __prescale  = _prescale;
  __vertexZ   = _vertexZ;

  for(int i = 0; i != _nTowers; ++i) {
    __towers.push_back(_towerId[i]);
    __towerDsmAdc.push_back(_towerDsmAdc[i]);
    __towerAdc.push_back(_towerAdc[i]);
    __towerEnergy.push_back(_towerEnergy[i]);
    __towerEt.push_back(_towerEt[i]);
  }

  for(int i = 0; i != _nJetPatches; ++i) {
    __jetPatches.push_back(_jetPatchId[i]);
    __jetPatchDsmAdc.push_back(_jetPatchDsmAdc[i]);
    __jetPatchAdc.push_back(_jetPatchAdc[i]);
    __jetPatchEnergy.push_back(_jetPatchEnergy[i]);
    __jetPatchEt.push_back(_jetPatchEt[i]);
  }
}


