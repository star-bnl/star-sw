// $Id: StJetTrgJPWriter.cxx,v 1.3 2008/07/13 09:37:52 tai Exp $
#include "StJetTrgJPWriter.h"

#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

#include <TFile.h>
#include <TTree.h>

#include <map>
#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;


void StJetTrgJPWriter::Init()
{
  _file->cd();
  _tree = new TTree(_treeName.c_str(), _treeTitle.c_str());

  _tree->Branch("runNumber"  , &_runNumber    , "runNumber/I"    );
  _tree->Branch("eventId"    , &_eventId      , "eventId/I"      );
  _tree->Branch("vertexZ"    , &_vertexZ      , "vertexZ/D"      );
  _tree->Branch("trigID"     , &_trigID       , "trigID/I"       );
  _tree->Branch("prescale"   , &_prescale     , "prescale/D"     );
  _tree->Branch("passed"     , &_passed       , "passed/I"         );
  _tree->Branch("nJetPatches", &_nJetPatches  , "nJetPatches/I"  );
  _tree->Branch("jetPatchId"   ,  _jetPatchId     , "jetPatchId[nJetPatches]/I");

  _trigID = _trgId;

}

void StJetTrgJPWriter::Make()
{

  _passed = (_uDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(_trgId) && _emcTrigMaker->isTrigger(_trgId));

  _runNumber = _uDstMaker->muDst()->event()->runId();
  _eventId = _uDstMaker->muDst()->event()->eventId();

  _vertexZ = _uDstMaker->muDst()->event()->primaryVertexPosition().z();

  _prescale = StDetectorDbTriggerID::instance()->getTotalPrescales()[_trgId];

  vector<int> jps;
  map<int,int> jetPatchMap = _emcTrigMaker->barrelJetPatchesAboveThreshold(_trgId);
  for(map<int,int>::const_iterator jp = jetPatchMap.begin(); jp != jetPatchMap.end(); ++jp) {
    jps.push_back(jp->first);
  }

  sort(jps.begin(), jps.end());

  _nJetPatches = jps.size();

  int i(0);
  for(vector<int>::const_iterator jp = jps.begin(); jp != jps.end(); ++jp) {
    _jetPatchId[i++] = *jp;
  }

  _tree->Fill();
}

void StJetTrgJPWriter::Finish()
{
  _tree->BuildIndex("runNumber", "eventId");
}
