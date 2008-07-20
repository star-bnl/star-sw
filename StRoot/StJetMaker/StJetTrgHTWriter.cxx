// $Id: StJetTrgHTWriter.cxx,v 1.4 2008/07/20 06:17:08 tai Exp $
#include "StJetTrgHTWriter.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

#include <TFile.h>
#include <TTree.h>

#include <map>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;


void StJetTrgHTWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  _tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  _tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  _tree->Branch("passed"     , &_passed       , "passed/I"         );
  _tree->Branch("hard"       , &_hard         , "hard/I"         );
  _tree->Branch("soft"       , &_soft         , "soft/I"         );
  _tree->Branch("nTowers"    , &_nTowers      , "nTowers/I"      );
  _tree->Branch("towerId"    ,  _towerId      , "towerId[nTowers]/I");

  _trigID = _trgId;

}

void StJetTrgHTWriter::Make()
{
  _hard = _uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId);

  _soft = _emcTrigMaker->isTrigger(_trgId);

  if(!(_hard || _soft)) return;

  _passed = (_hard && _soft);

  _runNumber = _uDstMaker->muDst()->event()->runId();

  _eventId = _uDstMaker->muDst()->event()->eventId();

  _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

  _prescale = StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];

  vector<int> towers;
  map<int,int> towerMap = _emcTrigMaker->barrelTowersAboveThreshold(_trgId);
  for(map<int,int>::const_iterator tower = towerMap.begin(); tower != towerMap.end(); ++tower) {
    towers.push_back(tower->first);
  }

  sort(towers.begin(), towers.end());

  _nTowers = towers.size();

  int i(0);
  for(vector<int>::const_iterator tower = towers.begin(); tower != towers.end(); ++tower) {
    _towerId[i++] = *tower;
  }

  _tree->Fill();
}

void StJetTrgHTWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
